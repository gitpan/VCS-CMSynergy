package VCS::CMSynergy;

our $VERSION = '@VERSION@';
# %version: 1.18 %

=head1 NAME

VCS::CMSynergy - Perl interface to Telelogic CM Synergy (aka Continuus/CM)

=head1 SYNOPSIS

  use VCS::CMSynergy;

  $ccm = VCS::CMSynergy->new(%attr);

  ($rc, $out, $err) = $ccm->ccm($ccm_command, @ccm_args);
  ($rc, $out, $err) = $ccm->any_ccm_command(@ccm_args); 

  $ary_ref = $ccm->query(@ccm_args);
  $ary_ref = $ccm->query_arrayref($query, @keywords);
  $ary_ref = $ccm->query_hashref($query, @keywords);
  $ary_ref = $ccm->query_object($query);

  $ary_ref = $ccm->finduse(@args);
  $path = $ccm->findpath($file_spec, $proj_vers);

  $ary_ref = $ccm->history(@ccm_args);
  $ary_ref = $ccm->history_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->history_hashref($file_spec, @keywords);

  $ary_ref = $ccm->ls(@ccm_args);
  $ary_ref = $ccm->ls_object($file_spec);
  $ary_ref = $ccm->ls_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->ls_hashref($file_spec, @keywords);

  $value = $ccm->get_attribute($attr_name, $file_spec);
  $ccm->set_attribute($attr_name, $file_spec, $value);
  $hash_ref = $ccm->list_attributes($file_spec);

  $delim = $ccm->delimiter;
  $database = $ccm->database;
  $ENV{CCM_ADDR} = $ccm->ccm_addr;

This synopsis only lists the major methods.

Methods that don't need a CM Synergy session are described
in L<VCS::CMSynergy::Client>. In fact, C<VCS::CMSynergy>
is derived from C<VCS::CMSynergy::Client>.

Methods for administering users and their roles are
described in L<VCS::CMSynergy::Users>. 

=cut

use 5.006_000;				# i.e. v5.6.0
use strict;

use VCS::CMSynergy::Client qw(
    is_win32 $Debug $Error $Ccm_command $OneArgFoo %new_opts 
    _exitstatus _error _usage);
our @ISA = qw(VCS::CMSynergy::Client);


use Carp;
use Config;
use Cwd;
use File::Spec;
use File::Temp qw(tempfile);		# in Perl core v5.6.1 and later
use Time::HiRes qw(gettimeofday tv_interval);

our %Use;
use VCS::CMSynergy::Object;

BEGIN
{
    if ($^O eq 'cygwin')
    { 
	eval "use Filesys::CygwinPaths qw(:all); 1" or die $@;
    }

    %Use =				# must be initialized at import time
    (
	tied_objects		=> undef,
	cached_attributes	=> undef,
    );
}

sub import
{
    my $class = shift;
    foreach (@_)
    {
	if (my ($opt) = /^[!:](.*)$/)
	{
	    $Use{$opt} = /^:/, next if exists $Use{$opt};
	}
	die "Invalid option `$_' in \"use ".__PACKAGE__."\"";
    }

    require VCS::CMSynergy::ObjectTieHash if $Use{tied_objects};
}

my %start_opts =
(
    KeepSession		=> undef,
    UseCoprocess	=> undef,
    CCM_ADDR		=> undef,
    ini_file		=> undef,
    remote_client	=> undef,
    database		=> "-d",
    home		=> "-home",
    host		=> "-h",
    password		=> "-pw",
    role		=> "-r",
    ui_database_dir	=> "-u",
    user		=> "-n",
);

sub new
{
    my ($class, %args) = @_;
    $class = ref $class if ref $class;

    my %new_args;
    foreach (keys %args)
    {
	$new_args{$_} = delete $args{$_} if exists $new_opts{$_};
    }
    return $class->_start(VCS::CMSynergy::Client->new(%new_args), %args);
}


sub _start
{
    my ($class, $client, %args) = @_;
    $class = ref $class if ref $class;
    croak("_start: $client is not a VCS::CMSynergy::Client")
	unless UNIVERSAL::isa($client, 'VCS::CMSynergy::Client');

    # make a deep clone of $client 
    my $self = { %$client };
    $self->{env} = { %{ $client->{env} } } if $client->{env};
    bless $self, $class;

    my @start = qw(start -m -q -nogui);
    while (my ($arg, $value) = each %args)
    {
	return $self->set_error("unrecognized attribute `$arg'") 
	    unless exists $start_opts{$arg};

	$self->{$arg} = $value;
	push @start, $start_opts{$arg} => $value if defined $start_opts{$arg};
    }

    $self->{env}->{CCM_ADDR} = delete $self->{CCM_ADDR} if defined $self->{CCM_ADDR};
    push @start, '-rc' if $self->{remote_client};

    if (defined $self->ccm_addr)
    {
	$self->{KeepSession} = 1 unless defined $self->{KeepSession};
	$Debug && $self->trace_msg("will keep session `".$self->ccm_addr."'\n");

	if (is_win32)
	{
	    # figure out user of session specified by CCM_ADDR
	    $self->{user} = 
		$self->ps(rfc_address => $self->ccm_addr)->[0]->{user};

	    # create a minimal ini file (see below for an explanation)
	    (my $inifh, $self->{ini_file}) = tempfile(SUFFIX => ".ini", UNLINK => 0);
	    $self->{ini_file} = fullwin32path($self->{ini_file}) if $^O eq 'cygwin';
	    			# because this name is passed down to ccm.exe
		
	    print $inifh "[UNIX information]\nUser = $self->{user}\n";
	    close($inifh);
	    push @{ $self->{files_to_unlink} }, $self->{ini_file};
	}
    }
    else
    {
	unless (defined $self->{ini_file})
	{
	    if (is_win32)
	    {
		# NOTES: 
		# (1) "ccm start -f nul ..." doesn't work on Windows
		#     (leads to error from ccm_seng), 
		#     so use an empty ini_file instead
		# (2) we can't use UNLINK=>1 with tempfile, because 
		#     the actual unlink may occur before the session is
		#     stopped and Windows refuses removing the "busy" file
		(undef, $self->{ini_file}) = tempfile(SUFFIX => ".ini", UNLINK => 0);
		$self->{ini_file} = fullwin32path($self->{ini_file}) if $^O eq 'cygwin';
		push @{ $self->{files_to_unlink} }, $self->{ini_file};
	    }
	    else
	    {
		$self->{ini_file} = File::Spec->devnull;
	    }
	}
	push @start, "-f", $self->{ini_file};

	$Ccm_command = $self->{ccm_command} = join(" ", @start);
	my $t0 = [ gettimeofday() ];
	my ($rc, $out, $err) = $self->exec($self->ccm_exe, @start);

	if ($Debug)
	{
	    my $elapsed = sprintf("%.2f", tv_interval($t0));
	    if ($Debug > 8)
	    {
		$self->trace_msg("<- ccm($self->{ccm_command})\n");
		$self->trace_msg("-> rc = $rc [$elapsed sec]\n");
		$self->trace_msg("-> out = \"$out\"\n");
		$self->trace_msg("-> err = \"$err\"\n");
	    }
	    else
	    {
		my $success = $rc == 0 ? 1 : 0;
		$self->trace_msg("ccm($self->{ccm_command}) = $success [$elapsed sec]\n");
	    }
	}

	return $self->set_error($err || $out) unless $rc == 0;

	$self->{env}->{CCM_ADDR} = $out;
	$Debug && $self->trace_msg("started session `$out'\n");
    }

    # NOTE: Use of $CCM_INI_FILE fixes the annoying `Warning:
    # Security violation.  User JLUSER is not authorized to the
    # Continuus interface at ...'  when running on Windows.
    #
    # Background: The problem is the obsolete ccm.ini file in
    # Windows' %SystemRoot%.  If ccm_gui or "ccm start ..." is
    # invoked _without_ specifying an ini file it writes the
    # Unix user (as given in the login popup or -n option, resp.)
    # into this file. If $CCM_INI_FILE is not set, all other "ccm ..."
    # invocations will read this file and check its "user"
    # entry against the session identified by $CCM_ADDR. If
    # they don't match, the above warning is issued and the
    # command aborted.  If we already have have an ini_file we
    # just set $CCM_INI_FILE to its name. Otherwise we fake
    # a minimal ini file with the correct setting of "user"
    # and set $CCM_INI_FILE to its name.
    #
    # NOTE: CM Synergy versions >= 6.0 on Windows do not use 
    # %SystemRoot%\ccm.ini any more. However, the problem persists:
    # if there's a [UNIX information] section in $CCM_HOME\etc\ccm.ini
    # or the user's personal ccm.ini its "User" setting will be used
    # and may trigger the "security violation".

    $self->{env}->{CCM_INI_FILE} = $self->{ini_file} if is_win32;

    if ($self->{UseCoprocess})
    {
	if ($self->{coprocess} = $self->_spawn_coprocess)
	{
	    $self->{cwd} = getcwd();	# remembers coprocess' working directory
	    $Debug && $self->trace_msg(
		"spawned coprocess (pid=".$self->{coprocess}->pid.")\n", 8);
	}
	else
	{
	    carp(__PACKAGE__." new: can't establish coprocess: $self->{error}\n" .
	         "-- ignoring UseCoprocess");
	}
    }

    # cache some info from database; this also doubles as a test for a valid session
    {
	my ($rc, $out, $err) = $self->_ccm(0, 'delimiter');
	return $self->set_error($err || $out) unless $rc == 0;
	$self->{delimiter} = $out;
    }

    $self->{objectname_rx} = qr/^(.*?)\Q$self->{delimiter}\E(.*?):(.*?):(.*?)$/;
    $self->{finduse_rx} = qr/(?m)^\t(.*?)\Q$self->{delimiter}\E.*?\@(.*?)$/;
    $self->{database} = undef;		# determine on demand

    if ($Debug >= 9)
    {
	require Data::Dumper;
	local $Data::Dumper::Useqq = 1;
	$self->trace_msg(Data::Dumper->Dump([$self], ["$self"]));
    }

    return $self;
}


sub DESTROY 
{
    my $self = shift;
    return unless $self->ccm_addr;	# session not yet established

    # NOTE: DESTROY might be called implicitly while unwinding 
    # stack frames during exception processing, e.g.
    #
    # eval {
    #   my $ccm = VCS::CMSynergy->new(...);
    #   ...
    #   die "D.O.A."			# <-- exception thrown
    #   ...
    # };
    # print "oops: $@\n" if $@;		# <-- handle it
    #
    # The exception causes a premature exit from the eval block.
    # But this block is also the scope of $ccm, hence $ccm->DESTROY
    # is called. Any eval block encountered during processing of DESTROY()
    # will reset $@  - even if no excpetion is thrown. Hence $@
    # might be empty at "print...". 
    # We localize $@ to avoid this unexpected behaviour.
    # FIXME: might be more correct to push localization into the
    # offending methods.
    local $@;
    
    $self->_kill_coprocess if $self->{coprocess};

    unless ($self->{KeepSession})
    {
	$self->_ccm(0, 'stop');
	$Debug && $self->trace_msg("stopped session ".$self->ccm_addr."\n");
    }

    # on Windows, certain files (e.g. the fake ccm.ini) might still be busy
    my @files_to_unlink;
    foreach (@{ $self->{files_to_unlink} })
    {
	unlink($_) or push @files_to_unlink, $_;
    }
    if (is_win32 && @files_to_unlink)
    {
        # wait a little, then try again
	sleep(2);
	unlink(@files_to_unlink);
    }

    %$self = ();			# paranoia setting
}

sub ccm_addr	
{ 
    return shift->{env}->{CCM_ADDR}; 
}


sub database	
{ 
    my $self = shift;

    unless (defined $self->{database})
    {
	# determine database path (in canonical format) from `ccm ps´
	my $ccm_addr = $self->ccm_addr;
	my $ps = $self->ps(rfc_address => $ccm_addr);
	return $self->set_error("can't find session `$ccm_addr' in `ccm ps'") 
	    unless $ps && @$ps > 0;
	$self->{database} = $ps->[0]->{database};
    }
    
    return $self->{database}; 
}

sub delimiter	
{ 
    return shift->{delimiter}; 
}


sub query
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, qw(query -u), @_);

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr

    return [ split(/\n/, $out) ] if $rc == 0;
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out);
}



sub query_arrayref
{
    my ($self, $query, @keywords) = @_;
    _usage(3, undef, '$query, $keyword...', \@_);

    return $self->_query($query, 0, @keywords);
}

sub query_hashref
{
    my ($self, $query, @keywords) = @_;
    _usage(3, undef, '$query, $keyword...', \@_);

    return $self->_query($query, 1, @keywords);
}


sub query_object
{
    my ($self, $query) = @_;
    _usage(2, 2, '$query', \@_);

    my $result =  $self->_query($query, 1, qw(object));
    return undef unless $result;

    # slice out the single "object" column
    return [ map { $_->{object} } @$result ];
}


sub query_object_with_attributes
{
    my ($self, $query, @attributes) = @_;
    _usage(2, undef, '$query, $attribute...', \@_);
    return $self->query_object($query) unless $Use{cached_attributes};

    my $result =  $self->_query($query, 1, qw(object), @attributes);
    return undef unless $result;

    # prime caches of the result objects
    my @objects;
    foreach my $row (@$result)
    {
	push @objects, $row->{object};

	my $acache = $row->{object}->_acache;

	# NOTE: Only cache attributes with defined values, because
	# an undefined value actually means that the attribute 
	# doesn't exist on the object.
	foreach (@attributes)
	{
	    $acache->{$_} = $row->{$_} if defined $row->{$_};
	}
    }
    return \@objects;
}


# helper: query with correct handling of multi-line attributes
sub _query
{
    my ($self, $query, $wanthash, @keywords) = @_;

    if (ref $query eq 'HASH')
    {
	$query = _query_shortcut($query);
	$Debug >= 2 && $self->trace_msg("query: $query\n");
    }

    my %want = map { $_ => "%$_" } @keywords;
    $want{object} = "%objectname" if $want{object};
    my $want_finduse = delete $want{finduse};

    # NOTE: We use \x01 and \x04 as record/field separators.
    # Change Synergy uses \x1C-\x1E in attribute
    # "transition_log" of "problem" objects, so these are out.
    # Also people have been known to enter strange characters
    # like \cG even when using a GUI exclusively.
    my $format = "\cA" . join("\cD", values %want) . "\cD";

    my ($rc, $out, $err) = $want_finduse ?
	$self->ccm_with_option(Object_format => $format, qw(finduse -query), $query) :
	$self->_ccm(0, qw(query -u -ns -nf -format), $format, $query);

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    foreach (split(/\cA/, $out))	# split into records 
    {
	next unless length($_);		# skip empty leading record

	my @cols = split(/\cD/, $_);
	my $row;

	if ($want_finduse)
	{
	    # parse finduse list (the last column)
	    my $finduse = pop @cols;
	    $row = $self->_parse_query_result(\%want, \@cols);

	    # finduse lines are of the forms
	    #
	    #   \t relative_path/object_name-version@project_name-project_version 
	    #   \t relative_path/object_name-version@project_name-project_version:project:project_instance
	    #
	    # which we parse into a hash
	    #   "objectname" => "relative_path/object_name"

	    # NOTE [DEPRECATE 4.5]: `Object not used'

	    # NOTE: Starting with CCM 6.3, project objects may have instances
	    # other than '1' (either for DCM reasosns, or because someone
	    # created a second project with the same name while the
	    # model attribute "multiple_local_proj_instances" was TRUE).
	    # CCM 6.3 apparently still returns "proj_vers" if instance='1' and
	    # the full objectname otherwise. We return the objectname
	    # in any case.

	    $row->{finduse} = {};
	    unless ($finduse =~ /Object is not used in scope|Object not used/)
	    {
		while ($finduse =~ /$self->{finduse_rx}/g)
		{
		    my ($path, $project) = ($1, $2);
		    $project .= ":project:1" unless $project =~ /:project:/;
		    $row->{finduse}->{$project} = $path;
		}
	    }
	}
	else
	{
	    $row = $self->_parse_query_result(\%want, \@cols);
	}

	push @result, $wanthash ? $row : [ @$row{@keywords} ];
    }

    return \@result;
}

sub _parse_query_result
{
    my ($self, $want, $cols) = @_;

    my %row;
    
    # translate "<void>" to undef and fill into correct slots
    @row{keys %$want} = map { $_ eq "<void>" ? undef : $_ } @$cols;
    
    # handle special keywords

    # Sigh. "ccm query -f %objectname" returns old-style fullnames
    # (i.e. "instance/cvtype/name/version") for certain types of 
    # objects, e.g. "cvtype" and "attype". But CM Synergy
    # doesn't accept these where a "file_spec" is expected 
    # (at least on Unix, because they contain slashes). 
    # Hence rewrite these fullnames to correct objectnames.

    if ($want->{objectname})
    {
        # rewrite fullname if necessary
	$row{objectname} =~ s{^ (.*?) / (.*?) / (.*?) / (.*?) $}
		             {$3$self->{delimiter}$4:$2:$1}x;
    }
    if ($want->{object})
    {
        # rewrite fullname if necessary
	(my $objectname = $row{object})
	    =~ s{^ (.*?) / (.*?) / (.*?) / (.*?) $}
		{$3$self->{delimiter}$4:$2:$1}x;

	# objectify column
	$row{object} = $self->object($objectname);
    }

    return \%row;
}

# helper (not a method): expand shortcut queries
# NOTE: CM Synergy seems to use the following quoting rules
# for the right hand side of an "attribute value clause" in a query:
# - string and text values must be quoted
# - boolean values ("TRUE" or "FALSE") must not be quoted
# - integer values must not be quoted, but must always have a leading sign
# - time values must be written as "time('Fri Dec 12 1997')"

sub _query_shortcut
{
    my $hashref = shift;
    my @clauses;

    while (my ($key, $value) = each %$hashref)
    {
	if (ref $value eq '')
	{
	    for ($key)
	    {
		/^task$/ && do 		# same as "ccm query -task ..."
		{
		    push @clauses, "is_associated_cv_of(cvtype = 'task' and task_number = '$value')";
		    next;
		};
		/^match$/ && do
		{
		    push @clauses, "name match '$value'";
		    next;
		};
		$value = "'$value'" unless $value =~ /^(TRUE|FALSE)$/;
		push @clauses, "$key = $value";
	    }
	}
	elsif (ref $value eq 'ARRAY')
	{
	    my $args = join(",", map { /^(TRUE|FALSE)$/ ? $_ : "'$_'" } @$value);
	    push @clauses, "$key($args)";
	}
	elsif (ref $value eq 'HASH')
	{
	    my $nested = _query_shortcut($value);
	    push @clauses, "$key($nested)";
	}
	else
	{
	    (my $method = (caller(1))[3]) =~ s/^.*:://;
	    croak("$method: dunno how to handle $key => ".(ref $value)." in shortcut query");
	}
    }

    return join(" and ", @clauses);
}


sub history
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, 'history', @_);
    return $self->set_error($err || $out) unless $rc == 0;

    return [ split(/^\*+\n?/m, $out) ];
}


sub history_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_history($file_spec, 0, @keywords);
}

sub history_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_history($file_spec, 1, @keywords);
}

# helper: history with correct handling of multi-line attributes
sub _history
{
    my ($self, $file_spec, $wanthash, @keywords) = @_;

    my %want = map { $_ => "%$_" } @keywords;
    $want{object} = "%objectname" if $want{object};
    my $want_predecessors = delete $want{predecessors};
    my $want_successors = delete $want{successors};

    my $format = "\cA" . join("\cD", values %want) . "\cD";

    my ($rc, $out, $err) = $self->_ccm(0, qw(history -f), $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    foreach (split(/\cA/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\cD/, $_);
	my $history = pop @cols;
	my $row = $self->_parse_query_result(\%want, \@cols);

	if ($want_predecessors || $want_successors)
	{
	    # parse history (the last column)
	    my ($predecessors, $successors) = $history =~
		/^Predecessors:\n\t?(.*)
		 ^Successors:\n\t?(.*)
		 ^\*
		/msx;

	    if ($want_predecessors)
	    {
		$row->{predecessors} = 
		    [ map { $self->object($_) } split(/\n\t?/, $predecessors) ];
	    }
	    if ($want_successors)
	    {
		$row->{successors} = 
		    [ map { $self->object($_) } split(/\n\t?/, $successors) ];
	    }
	}

	push @result, $wanthash ? $row : [ @$row{@keywords} ];
    }

    return \@result;
}


sub finduse
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, 'finduse', @_);

    # NOTE: `ccm finduse ...' without `-query' complains if some of 
    # the given objects do not exist (and exits with status 1 unless at least
    # one exists). But for `ccm finduse -query ...', if there are no hits, 
    # the command exits with status 1 and produces no output on either 
    # stdout and stderr. (This is the same behaviour as for `ccm query ...'.) 
    # We will not produce an error in any case. However, the returned array
    # may contain fewer elements than file_specs given as arguments.

    if ($rc == 0)
    {
	my (@result, $uses);
	foreach (split(/\n/, $out))
	{
	    # ignore complaints about non-existing objects 
	    # and the dummy "use" line printed if object is not used anywhere
	    # NOTE [DEPRECATE 4.5]: `Object not used'
	    next if /Object version could not be identified|Object is not used in scope|Object not used/;

	    # a usage line is matched by finduse_rx
	    if (/$self->{finduse_rx}/)
	    {
		my ($path, $project) = ($1, $2);
		$project .= ":project:1" unless $project =~ /:project:/;
		$uses->{$project} = $path;
		next;
	    }

	    # otherwise the line describes an object satisfying the query
	    # in the format given by option `Object_format' (default:
	    # "%displayname %status %owner %type %project %instance %task")
	    push(@result, [ $_, $uses = {} ]);
	}
	return \@result;
    }
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out);
}

sub findpath
{
    my ($self, $file_spec, $proj_vers) = @_;
    my $finduse = $self->finduse($file_spec);
    return undef unless defined $finduse;
    return $self->set_error("`$file_spec´ matches more than one object") unless @$finduse == 1;
    return $finduse->[0]->[1]->{$proj_vers};
}



{
    package VCS::CMSynergy::Traversal;
    our (@dirs, @projects, $prune);
}

sub traverse_project
{
    my ($self, $wanted, $project, $dir) = @_;
    _usage(3, 4, '{ \\&wanted | \\%wanted }, $project [, $dir_object]', \@_);

    if (ref $wanted eq 'CODE')
    {
	$wanted = { wanted => $wanted };
    }
    elsif (ref $wanted eq 'HASH')
    {
	return $self->set_error("traverse_project: argument 1: option `wanted' is mandatory")
	    unless exists $wanted->{wanted};
	foreach (qw(wanted preprocess postprocess))
	{
	    return $self->set_error("traverse_project: argument 1: option `$_' must be a CODE ref") 
		if exists $wanted->{$_} && ref $wanted->{$_} ne 'CODE';
	}
	return $self->set_error("traverse_project: argument 1: option `attributes' must be an ARRAY ref") 
	    if exists $wanted->{attributes} && ref $wanted->{attributes} ne 'ARRAY';
    }
    else
    {
	return $self->set_error("traverse_project: argument 1 must be a CODE or HASH ref");
    }
    $wanted->{attributes} ||= [];

    if (ref $project)
    {
	return $self->set_error("argument 2 `$project' must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($project, "VCS::CMSynergy::Object");
	return $self->set_error("argument 2 `$project' must have type `project'")
	    unless $project->cvtype eq "project";
    }
    else
    {
	# treat $project as project_version string or an objectname
	$project .= ":project:1" unless $project =~ /:project:1$/;
	$project = $self->object($project);
    }

    if (defined $dir)
    {
	return $self->set_error("argument 3 `$dir' must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($dir, "VCS::CMSynergy::Object");
	return $self->set_error("argument 3 `$dir' must have cvtype `dir'")
	    unless $dir->cvtype eq "dir";

	# check that $dir is member of $project
	my $result = $self->query_object_with_attributes(
	    {
		name		=> $dir->name,
		cvtype		=> $dir->cvtype,
		instance	=> $dir->instance,
		version		=> $dir->version,
		is_member_of	=> [ $project ]
	    },
	    @{ $wanted->{attributes} });
	return $self->set_error("directory `$dir' doesn't exist or isn't a member of `$project'")
	    unless @$result;
	$dir = $result->[0];
    }
    else
    {
	my $result = $self->query_object_with_attributes(
	    { 
		name		=> $project->name,
		cvtype		=> $project->cvtype,
		instance	=> $project->instance,
		version		=> $project->version
	    }, 
	    @{ $wanted->{attributes} });
	return $self->set_error("project `$project' doesn't exist")
	    unless @$result;
	$dir = $result->[0];
    }

    local @VCS::CMSynergy::Traversal::projects = ($project);
    local @VCS::CMSynergy::Traversal::dirs = (); 
    $self->_traverse_project($wanted, $project, $dir);
}

sub _traverse_project
{
    my ($self, $wanted, $project, $parent) = @_;

    my $children = $self->query_object_with_attributes(
	{ is_child_of => [ $parent, $project ] }, 
	@{ $wanted->{attributes} });

    if ($wanted->{preprocess})
    {
        # make $_ the current dir/project during preprocess'ing 
	local $_ = $parent;
	{ $children = [ $wanted->{preprocess}->(@$children) ]; }
    }

    if (!$wanted->{bydepth}) 
    {
	local $_ = $parent;
	local $VCS::CMSynergy::Traversal::prune = 0;
	{ $wanted->{wanted}->(); }		# protect against wild "next"
	return if $VCS::CMSynergy::Traversal::prune;
    }

    unless ($parent->cvtype eq 'project')
    {
	push @VCS::CMSynergy::Traversal::dirs, $parent;
    }

    foreach (@$children)			# localizes $_
    {
	if ($_->cvtype eq "project" && $wanted->{subprojects})
	{
	    push @VCS::CMSynergy::Traversal::projects, $_;
	    $self->_traverse_project($wanted, $_, $_);
	    pop @VCS::CMSynergy::Traversal::projects;
	    next;
	}
	if ($_->cvtype eq "dir")
	{
	    $self->_traverse_project($wanted, $project, $_);
	    next;
	}

	{ $wanted->{wanted}->(); }
    }

    unless ($parent->cvtype eq 'project')
    {
	pop @VCS::CMSynergy::Traversal::dirs;
    }
    
    if ($wanted->{bydepth}) 
    {
	local $_ = $parent;
	local $VCS::CMSynergy::Traversal::prune = 0;
	{ $wanted->{wanted}->(); }
	return if $VCS::CMSynergy::Traversal::prune;
    }

    if ($wanted->{postprocess})
    {
        # make $_ the current dir/project during postprocess'ing 
	local $_ = $parent;
	{ $wanted->{postprocess}->(); }
    }
}


sub get_attribute
{
    my ($self, $attr_name, $file_spec) = @_;
    _usage(3, 3, '$attr_name, $file_spec', \@_);

    my ($rc, $out, $err) = $self->_ccm(0, 'attribute', -show => $attr_name, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;
    return $out;
}


sub set_attribute
{
    my ($self, $attr_name, $file_spec, $value) = @_;
    _usage(4, 4, '$attr_name, $file_spec, $value', \@_);

    # use ye olde text_editor trick if $value may cause problems
    # (depending on execution mode and platform) because its
    # too long or contains unquotable characters or...
    my ($rc, $out, $err);
    if (($self->{coprocess} && (length($value) > 1600 || $value =~ /["\r\n]/)) ||
        (is_win32 && (length($value) > 100 || $value =~ /[%<>&"\r\n]/)))
    {
	($rc, $out, $err) = $self->ccm_with_text_editor($value, 
	    'attribute', -modify => $attr_name, $file_spec);
    }
    else
    {
	($rc, $out, $err) = $self->_ccm(0,
	    'attribute', -modify => $attr_name, -value => $value, $file_spec);
    }
    return $self->set_error($err || $out) unless $rc == 0;
    return $value;
}


sub create_attribute
{
    my ($self, $name, $type, $value, @file_specs) = @_;
    _usage(5, undef, '$name, $type, $value, $file_spec...', \@_);

    my @args = (-type => $type, @file_specs);
    unshift @args, -value => $value if defined $value;
    # FIXME this should employ the same heuristic as set_attribute()
    # and use a separate ccm_with_text_editor(..., 'attribute -modify', ...)
    # for troublesome $value

    return $self->ccm('attribute', -create => $name, @args);
}

sub delete_attribute
{
    my ($self, $name, @file_specs) = @_;
    _usage(3, undef, '$name, $file_spec...', \@_);

    return $self->ccm('attribute', -delete => $name, @file_specs);
}

sub copy_attribute
{
    my ($self, $name, $flags, $from_file_spec, @to_file_specs) = @_;
    _usage(5, undef, '$name, \\%flags, $from_file_spec, $to_file_spec...', \@_);

    $name = join(':', @$name) if ref $name;

    my @args = ($from_file_spec, @to_file_specs);
    unshift @args,  map { "-$_" } @$flags if $flags;

    return $self->ccm('attribute', -copy => $name, @args);
}

sub list_attributes
{
    my ($self, $file_spec) = @_;
    _usage(2, 2, '$file_spec', \@_);

    my ($rc, $out, $err) = $self->_ccm(0, 'attribute', -la => $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my %attrs = $out =~ /^(\S+) \s* \( (.*?) \)/gmx;
    return \%attrs;
}

sub property
{
    my ($self, $keyword, $file_spec) = @_;
    _usage(3, 3, '$keyword, $file_spec', \@_);

    my ($rc, $out, $err) = 
	$self->_ccm(0, qw(properties -nf -format), "%$keyword", $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    return $out eq "<void>" ? undef : $out;
}


sub ls
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, 'ls', @_);
    return $self->set_error($err || $out) unless $rc == 0;

    return [ split(/\n/, $out) ];
}

sub ls_object
{
    my ($self, $file_spec) = @_;
    $file_spec = '.' unless defined $file_spec;

    my $rows = $self->ls(qw(-f %objectname), $file_spec);
    return undef unless $rows;
    return [ map { $self->object($_) } @$rows ];
}

sub ls_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    my $ary_ref = $self->ls_hashref($file_spec, @keywords);
    return unless $ary_ref;
 
    return [ map { [ @$_{@keywords} ] } @$ary_ref ];
}

sub ls_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    my $format = join("\a", map { "%$_" } @keywords);

    my $rows = $self->ls(qw(-f), $format, $file_spec);
    return undef unless $rows;

    my @result;
    foreach (@$rows)
    {
	my %hash;
	@hash{@keywords} = map { $_ eq "<void>" ? undef : $_ } 
			       split(/\a/, $_);

	push(@result, \%hash);
    }
    return \@result;
}

    
sub set
{
    my ($self, $option, $value) = @_;
    _usage(1, 3, '[$option [, $value]]', \@_);

    if (@_ == 1)
    {
	my ($rc, $out, $err) = $self->_ccm(0, 'set');
	return $self->set_error($err || $out) unless $rc == 0;

	my %options;
	while ($out =~ /^(\S+) = (.*)$/gm)
	{
	    $options{$1} = $2 eq "(unset)" ? undef : $2;
	}
	return \%options;
    }

    my ($rc, $out, $err);
    my $old_value;

    # no need to get old value if we are called in void context
    if (defined wantarray)
    {
	my ($rc, $out, $err) = $self->_set($option);
	return $self->set_error($err || $out) unless $rc == 0;
	$old_value = $out;
    }

    if (@_ == 3)
    {
	my ($rc, $out, $err) = $self->_set($option, $value);
	return $self->set_error($err || $out) unless $rc == 0;
    }
    
    return $old_value;
}

sub _set
{
    my ($self, $option, $new_value) = @_;

    if (@_ == 2)
    {
	my ($rc, $out, $err) = $self->_ccm(0, set => $option);
	$out = undef if $rc == 0 &&  $out eq "(unset)";
	return ($rc, $out, $err);
    }

    if (@_ == 3)
    {
	my ($rc, $out, $err) = defined $new_value ?
	    $self->_ccm(0, set => $option, $new_value) :
	    $self->_ccm(0, unset => $option);
	return ($rc, $out, $err);
    }
    
    return _error("wrong number of arguments");
}


# helper: save value of $option, set it to $new_value, 
#  call _ccm(0, @args), restore $option; returns ($rc, $out, $err)
#  (usually the return value from _ccm(@args) except there were errors
#  in setting the option)
sub ccm_with_option
{
    my ($self, $option, $new_value, @args) = @_;

    my ($rc, $out, $err) = $self->_set($option);
    return ($rc, $out, $err) unless $rc == 0;
    my $old_value = $out;

    ($rc, $out, $err) = $self->_set($option, $new_value);
    return ($rc, $out, $err) unless $rc == 0;

    my ($ccm_rc, $ccm_out, $ccm_err) = $self->_ccm(0, @args);

    ($rc, $out, $err) = $self->_set($option, $old_value);

    return ($ccm_rc, $ccm_out, $ccm_err) if $rc == 0;
    return ($rc, $out, $err);
}

# helper: implements ye olde text_editor trick for ccm commands
# that would interactively open an editor in order to let the user modify
# some (text) value; ccm_with_text_editor writes $text_value 
# to a temporary file, then calls ccm_with_option with
# text_editor="cp temporary_file %filename" and returns its results
# calls $self->_ccm(@args).
sub ccm_with_text_editor
{
    my ($self, $text_value, @args) = @_;

    my $text_file = $self->{text_file};
    unless (defined $text_file)
    {
	(undef, $text_file) = tempfile(SUFFIX => ".dat");
	return _error("can't create temp file to set text value: $!")
	    unless defined $text_file;

	push @{ $self->{files_to_unlink} }, $text_file;
	$self->{text_file} = $text_file;
    }

    local *TEXT;
    open(TEXT, ">$text_file")
	or return _error("can't open temp file `$text_file' to set text value: $!");
    print TEXT $text_value;
    close(TEXT);

    # NOTE: 
    # (1) $text_file is safe wrt cygwin, because $Config{cp} is
    #     a cygwin program ("/usr/bin/cp") on cygwin.
    # (2) $Config{cp} is "copy" on Win32, but CMSynergy seems unable to
    #     to execute "shell" builtins, hence use "xcopy" instead
    #     (use "/y" to overwite files without prompting)
    return $self->ccm_with_option(
	text_editor => $^O eq 'MSWin32' ?
	    qq[xcopy /y /q "$text_file" "%filename"] :
	    qq[$Config{cp} '$text_file' '%filename'],
	@args);
}


sub get_releases
{
    my ($self) = @_;

    my ($rc, $out, $err) = $self->_ccm(0, qw(releases -show));
    return $self->set_error($err || $out) unless $rc == 0;

    my %releases;
    foreach (split(/\n/, $out))
    {
	next if /^\s*$/;
	my ($release, @names) = split(/\s*[:,]\s*/);
	$releases{$release} = [ @names ];
    }
    return \%releases;
}


sub set_releases
{
    my ($self, $releases) = @_;
    _usage(2, 2, '\\%releases', \@_);

    my $text = "";
    {
	local $" = ", ";
	while (my ($release, $names) = each %$releases) 
	{
	    $text .= "$release: @$names\n";
	}
    }

    my ($rc, $out, $err) =
	$self->ccm_with_text_editor($text, qw(releases -edit));

    return $rc == 0 || $self->set_error($err || $out);
}


# generic wrapper for undefined method "foo":
# 	$ccm->foo(@args)
# gets turned into
# 	$ccm->ccm("foo", @args)
# in fact, we create a method `foo' on the fly with this definition
sub AUTOLOAD
{
    my ($this) = @_;

    our $AUTOLOAD;

    # NOTE: the fully qualified name of the method has been placed in $AUTOLOAD
    my ($class, $method) = $AUTOLOAD =~ /^(.*)::([^:]*)$/;
    return if $method eq 'DESTROY'; 

    # we don't allow autoload of class methods
    croak("Can't locate class method \"$method\" via class \"$class\"")
	unless ref $this;
    $Debug >= 2 && $this->trace_msg("autoloading method \"$method\"\n");

    # create the new method on the fly
    no strict 'refs';
    *{$method} = sub 
    {
	my $self = shift;

	my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, $method, @_);

	return wantarray ? ($rc, $out, $err) : 1 if $rc == 0;
	return $self->set_error($err || $out, undef, 0, $rc, $out, $err);
    };

    # call it w/o pushing a new stack frame (with same parameters)
    goto &$method;
}


# test whether session is still alive (without causing an exception)
sub ping
{
    my ($rc) = shift->_ccm(0, 'delimiter');
    return $rc == 0;
}

# $ccm->object(objectname) => VCS::CMSynergy::Object
# $ccm->object(name, version, cvtype, instance) => VCS::CMSynergy::Object
sub object
{
    my $self = shift;

    croak(__PACKAGE__."::object: invalid number of arguments" .
          "\n  usage: \$ccm->object(\$name, \$version, \$cvtype, \$instance)" .
          "\n  or     \$ccm->object(\$objectname)")
	unless @_ == 1 || @_ == 4;
    
    return VCS::CMSynergy::Object->new($self, @_) if @_ == 4;

    my $objectname = shift;
    return VCS::CMSynergy::Object->new($self, $1, $2, $3, $4)
	if $objectname =~ /$self->{objectname_rx}/;

    return $self->set_error("invalid objectname `$objectname'");
}

# $ccm->object_other_version(object, version) => VCS::CMSynergy::Object
#	new Object with same name/cvtype/instance as OBJECT, but version VERSION
sub object_other_version
{
    my ($self, $object, $other_version) = @_;
    return $self->object($object->name, $other_version, $object->cvtype, $object->instance);
}

1;

__END__

=head1 DESCRIPTION

  use VCS::CMSynergy;

  my $ccm = VCS::CMSynergy->new(database => "/ccmdb/test/tut62/db");

  $ccm->checkout(qw(foo/bar.c@foo~user -to test))
    or die "checkout failed: ".$ccm->error;

  my $csrcs = $ccm->query_hashref("type = 'csrc'",
				  qw(displayname modify_time));
  if ($csrcs)
  {
    print "$_->{displayname} $_->{modify_time}\n" foreach (@$csrcs);
  }

=head1 OPTIONS

The following optional features can be enabled at compile time
with the notation

  use VCS::CMSynergy ':option';

=head2 :cached_attributes

This causes L<VCS::CMSynergy::Object>s to keep a cache of 
attribute names and values. The cache is only maintained for those
attributes that are actually accessed by the program. See
L<VCS::CMSynergy::Object/ATTRIBUTE METHODS> for a list of
methods perusing this cache.

Note that this cache
is only maintained if you use L<VCS::CMSynergy::Object> methods
(including the L<VCS::CMSynergy::Object/TIEHASH INTERFACE>) 
and will get inconsistent if you
mix C<VCS::CMSynergy::Object> and C<VCS::CMSynergy> calls
on the same object.

=head2 :tied_objects

If this option is in effect.
you can use a C<VCS::CMSynergy::Object> in the same way you
would use a hash reference. The available keys are the underlying
CM Synergy object's attributes. 
See L<VCS::CMSynergy::Object/TIEHASH INTERFACE> for details.

=head1 GENERAL METHODS

=head2 new

  my $ccm = VCS::CMSynergy->new( database => "/ccmdb/foo/db" )
              or die VCS::CMSynergy->error;

Starts a new CM Synergy session. Returns a session handle if it succeeds. 

If it fails to start a session, it returns C<undef>. Use
C<< VCS::CMSynergy->error >> to get the error string printed by CM Synergy.

Multiple simultaneous sessions to multiple databases or with
engines running on different hosts, even using different versions
of CM Synergy, are supported.

C<new> issues a B<ccm start> command and remembers the C<CCM_ADDR>
in the session object (together with other session state).
The session is stopped (B<ccm stop>) when the session object
is destroyed (see L</DESTROY>).

C<new> is called with an attribute hash. The following attributes
are currently supported:

=over 4

=item C<database> (string)

CM Synergy database path. 

This is the only attribute required on Unix systems.

=item C<host> (string)

CM Synergy engine host to use.

It defaults to the local host.

=item C<role> (string)

User's initial CM Synergy role.

It defaults to C<developer>.

=item C<user> (string)

CM Synergy user. 

This attribute is available and required on Windows systems only.

=item C<password> (string)

User's password. 

This attribute is required on Windows systems or when using
ESD to connect to the CM Synergy engine.

=item C<ini_file> (string)

CM Synergy ini file to use. 

In contrast to the CM Synergy B<ccm start> command there is I<no>
default ini file consulted. (On Unix systems this is achieved
by executing B<ccm start> with the option C<-f /dev/null>.) The reason
is that we want scripts to behave in a reproducible way. Otherwise
the script might accidentally work with the current contents of
the current user's ini file, but might fail when invoked by another user.
Or it might fail when invoked by the same user at a later time because of
changes to her ini file (e.g. because of another session between
invocations of the script). So if you really want to rely on an ini file,
you have to supply it explicitly.

=item C<CCM_ADDR> (string)

Specifies the RFC address of an established CM Synergy session.

If you specify this attribut L</new> does not create a new session,
but will attach to the one specified. Also, implicitly sets C<KeepSession>
to "on" so that destruction of the new session
handle will not cause a B<ccm stop>. However, setting C<KeepSession> 
explicitly will take precedence.

Note that there is no default value. In particular, L</new> ignores
the environment variable of the same name.

=item C<CCM_HOME> (string)

Value of the C<CCM_HOME> environment variable to use for this session.

It defaults from the environment variable of the same name,
i.e. C<$ENV{CCM_HOME}>.

This is only of interest if you have multiple version of CM Synergy
installed. You can have simultaneous sessions using different
CM Synergy versions (the module takes care of setting the C<CCM_HOME>
variable appropriately before issuing any C<ccm> commands). 

=item C<ui_database_dir> (string)

Specifies the path name to which your database information is copied 
when you are running a remote client session. This corresponds
to the C<-u pathname> option for B<ccm start>.

Note: This option is particularly useful for Windows clients. If L</new>
fails with something like 

  Server Database Path ... is not accessible from this Client.   
  Please specify a Client Database Path

you should specify this option with a local directory path, e.g.

  my $ccm = VCS::CMSynergy->new(..., ui_database_dir => 'c:\\temp', ...);

The value is  what you would enter under 
"Client Information"/"Database Path" in the GUI's "Startup Info" window.
Or you can set B<ui_database_dir> in the [Options] section of 
the system ini file (note that setting it in your personal ini file
won't do, as this file is I<not> read by L</new> by default).

=item C<remote_client> (boolean)

If the value is "on", it specifies that you want to start the CM Synergy
session as a remote client. This corresponds to the C<-rc> option for
B<ccm start>. This option is only useful on Unix systems. It defaults
to "off".

=item C<PrintError> (boolean)

This attribute can be used to force errors to generate warnings (using
L<carp|Carp/carp>) in addition to returning error codes in the normal way.  
When set to true, any method which results in an error occuring will cause
the corresponding C<< $ccm->error >> to be printed to stderr.

It defaults to "on".

Note: L</PrintError> and L</RaiseError> below are stolen from the excellent
L<DBI> module.

=item C<RaiseError> (boolean)

This attribute can be used to force errors to raise exceptions 
(using L<croak|Carp/croak>) rather than simply return error codes in the normal way. 
When set to true, any method which results in an error will cause
effectively a C<die> with the actual C<< $ccm->error >>
as the message. 

It defaults to "off".

If you turn C<RaiseError> on then you'd normally turn C<PrintError> off.
If C<PrintError> is also on, then the C<PrintError> is done first (naturally).

Typically C<RaiseError> is used in conjunction with C<eval { ... }>
to catch the exception that's been thrown and followed by an
C<if ($@) { ... }> block to handle the caught exception. 

If you want to temporarily turn C<RaiseError> off (inside a library function
that is likely to fail, for example), the recommended way is like this:

  {
    local $ccm->{RaiseError};  # localize and turn off for this block
    ...
  }

The original value will automatically and reliably be restored by Perl,
regardless of how the block is exited.
The same logic applies to other attributes, including C<PrintError>.

=item C<HandleError> (code ref)

This attribute can be used to provide your own
alternative behaviour in case of errors. If set to a
reference to a subroutine then that subroutine is called
when an error is detected (at the same point that
L</RaiseError> and L</PrintError> are handled).

The subroutine is called with three parameters: the
error message string that L</RaiseError> and L</PrintError>
would use, the C<VCS::CMSynergy> object being used, and the 
value being returned by the method that failed (typically undef).

If the subroutine returns a false value then the
L</RaiseError> and/or L</PrintError> attributes are checked
and acted upon as normal. Otherwise the error is considered "handled"
and execution proceeds normally with a return from the method.

For example, to "die" with a full stack trace for any error:

  use Carp;
  $ccm->{HandleError} = sub { confess(shift) };

=item C<KeepSession> (boolean)

If this attribute is "on" then destruction of the new session handle
will not cause a B<ccm stop>. 

This may be used if you want to
create a new CM Synergy session in one program and then re-use it
in another program (since session creation is a rather time consuming
operation). In this case you should use C</ccm_addr> to extract
the session's RFC address (after C</new> returns) and somehow pass it
on to the other program.

It defaults to "off" unless you also specify C<CCM_ADDR>.

=item C<UseCoprocess> (boolean)

This feature is highly experimental, B<use it at your own risk>.

B<You must have the L<Expect> module installed to use this feature.>
(Since L<Expect> is not available for Win32 systems, 
C<UseCoprocess> is ignored there.)

If C<UseCoprocess> is "off", C<VCS::CMSynergy.pm> executes a separate
C<ccm> process whenever it invokes the CM Synergy CLI, e.g.

  $ccm->checkout('foo.c');
  $ccm->set_attribute('color', 'foo.c', 'blue');
  $csources = $ccm->query("name match '*.c'");

results in the execution of the following three processes:

  ccm checkout foo.c
  ccm attribute -modify color -value blue foo.c
  ccm query "name match '*.c'"

In particular, we incur the startup overhead of B<ccm> three times.
This overhead is noticable, esp. if you are doing 
lots of CM Synergy operations.

If C<UseCoprocess> is "on", only one B<ccm> process per CM Synergy
session ever gets executed. The way it works is that
C<< VCS::CMSynergy->new >> starts an "interactive"
(i.e. one invoked without arguments) B<ccm> process in the background.
Later invocations of the CM Synergy CLI pipe their commands to its input and 
read back the output (up to the next C<< "ccm>" >> prompt). 
The actual command is then followed in the same way by C<set error>
to retrieve the success status. Destruction  of the session object
will cause termination of this "coprocess" (via "stop" or "exit" depending
on the setting of L</KeepSession>).

The "coprocess" method avoids the startup overhead, but may run into 
other problems:

=over 4

=item *

The "interactive" B<ccm> imposes stricter limits 
on the length of one CLI command (experimentally put at ~2000 bytes)
than the "batch" B<ccm> (where the limit on the arguments of a process
is typically imposed by the operating system). Moreover, it will
silently truncate the command and not signal an error (unless the
truncation causes a syntax error).

=item *

The current method to communicate with the "coprocess" does not allow
for separation of its stdout and stderr.

=item *

C<UseCoprocess> does not work under Win32 at all.

=back

The default value of C<UseCoprocess> is "off".

=back

=head2 DESTROY

  $ccm->DESTROY;

Stops the CM Synergy session represented by the session handle
by executing B<ccm stop> (unless the session has the C<KeepSession>
attribut set).

You should never call this method explicitly, as it
is invoked by the Perl runtime when the Perl process exits
(either by calling C<exit> or because of a C<die>).
Hence, a script using the C<VCS::CMSynergy> module will not leave
any CM Synergy sessions hanging around. 

Actually, the Perl runtime will call C<DESTROY> when the last reference
to a session handle goes out of scope, so in the following example
each session will be stopped as soon as one loop through the C<foreach>
body is completed, i.e. there is at most one session in progress
at any one time:

  my @databases = ...;		# a list of CM Synergy databases
  foreach my $db (@databases)
  {
    my $ccm = VCS::CMSynergy->new( database => $db, ... );
    ...
    # perform some operation on $db
    ...
    # session is stopped as "my" variable $ccm is about to go out of scope
  }

Note: The correct way to explicitly stop a session is neither

  $ccm->stop;

nor is it

  $ccm->DESTROY;

Though both forms will execute B<ccm stop>,
the first form makes C<$ccm> a C<VCS::CMSynergy> object with an invalid
RFC address (i.e. attribute CCM_ADDR), while the second form leaves you with an
"empty"  C<VCS::CMSynergy> object. Instead, you should rather say

  $ccm = undef;

=head2 ccm

  ($rc, $out, $err) = $ccm->ccm($command, @args);

This is the workhorse of the VCS::CMSynergy module. It executes B<ccm> 
with command C<$command> and (optional) parameters C<@args>.
In array context it returns a three-element array consisting of
the (operating system) exit code of B<ccm>, and what B<ccm> printed
on stdout and stderr. Note that the exit code is 0 if B<ccm>
operated successfully. On DOSish operating systems the 
(possibly multi-line) strings C<$out> and C<$err> have been read
by Perl in "text" mode, i.e. contain LF characters instead of CRLF.
In any case, C<$out> and C<$err> have been C<chomp>ed.

In scalar context C<ccm> returns the
"logical" exit code, i.e. C<!$rc>, so that you can write:

  $ccm->ccm('checkout', $file_spec) 
      or die "checkout failed: ".$ccm->error;

Note that you must pass every C<ccm> argument or option as a single Perl 
argument. For literal arguments the C<qw()> notation may come in handy, e.g.

  ($rc, $out, $err) = $ccm->ccm(qw(finduse -state working));

Most specialized methods in the VCS::CMSynergy module are ultimately implemented
via the L</ccm> method. Using it directly is only recommended for
commands that perform some action, e.g. B<ccm checkout>, as opposed to 
query-like commands. For the latter, e.g. B<ccm query>, use one of the 
methods that return the information in structured form, 
e.g. L<query_arrayref|/"query_arrayref, query_hashref"> or
L<query_hashref|/"query_arrayref, query_hashref">, instead of having 
to parse C<$out> yourself.

In fact, there is a shortcut for "action" commands: if you call
a non-existent method on a VCS::CMSynergy object, it tries to invoke
the L</ccm> method with the original method name as the C<$command>
followed by the parameters of the original call, i.e.

  $ccm->checkout($file_spec);

and 

  $ccm->ccm('checkout', $file_spec);

are equivalent (given that there is no real C<checkout> method).
Return values are those of L</ccm> (depending on context).
This is accomplished by a suitable C<AUTOLOAD> method.

=head1 QUERY METHODS

=head2 query

  $ary_ref = $ccm->query(@args);

Executes the B<ccm query> command with the given C<@args> as parameters.
The output (as formatted by the C<-format> option) is split into lines.
These are L<chomp|perlfunc/chomp>ed and a reference to the resulting array
of strings is returned. 

If there a no hits, a reference to an empty
array is returned. (Note that B<ccm query> considers this an error,
but the VCS::CMSynergy module does not.) 

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm query> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.
Example:

  $result = $ccm->query(qw(-t csrc -f), '%displayname %modify_time');
  print "$_\n" foreach (@$result);

If you are interested in the value of several attributes for the
result set of the query, you should look at the 
L<query_arrayref|/"query_arrayref, query_hashref"> and 
L<query_hashref|/"query_arrayref, query_hashref"> methods that 
return this information in 
structured form. If you are only interested in the identity of
objects in the result set, you should look at the 
L<query_object|/"query_object, query_object_with_attributes"> method.

Note that L</query> will probably produce
unpredictable results when the C<-format> option references attributes
that can have multi-line values, e.g. C<status_log>. 
L<query_arrayref|/"query_arrayref, query_hashref"> and
L<query_hashref|/"query_arrayref, query_hashref"> handle this case correctly.

=head2 query_arrayref, query_hashref

  $ary_ref = $ccm->query_arrayref($query, @keywords);
  print "@$_\n" foreach @$ary_ref;

  $ary_ref = $ccm->query_hashref($query, @keywords);
  print "@$_{@keywords}\n" foreach @$ary_ref;

C<query_arrayref> and C<query_hashref>
execute B<ccm query> with the query expression C<$query> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. They both return a reference to an array of references,
one per result row. 

C<query_arrayref> represents a row as an array containing
the values of the keywords for that particular object in the result set
(in the order given by C<@keywords>). 

C<query_hashref> represents a row as a hash containing
attribute and value pairs where the keys are the C<@keywords>.

If the query returned no hits, both C<query_arrayref>
and C<query_hashref> return a reference to an empty array.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array or hash element is C<undef> (whereas B<ccm query> would print it as
the string C<< "<void>" >>).

The following names may also be used as keywords though they
are neither built-in nor attributes:

=over 4

=item C<object>

The value is a C<VCS::CMSynergy::Object> representing 
the object in the result set.

=item C<finduse>

The value is a reference to a hash identifying in what parts of what
projects the object is used.  A key in the hash is the project's objectname.
The hash value is the
corresponding relative path (including the object's name) in the project.
This information is the same as reported by B<ccm finduse>. In fact, if
this keyword is given, L<query_arrayref|/"query_arrayref, query_hashref"> 
and L<query_hashref|/"query_arrayref, query_hashref">
invoke B<ccm finduse -query $query> rather than B<ccm query $query>.  
Example:

  my $result = $ccm->query_arrayref(
    "name = 'main.c'", qw(objectname finduse));

returns (as formatted by L<Data::Dumper>):

  $result = [
    [
      'main.c-1:csrc:3',	# objectname
      {				# finduse
	 'guilib-1.0'	=> 'guilib/sources/main.c',
	 'guilib-int'	=> 'guilib/sources/main.c',
	 'guilib-darcy'	=> 'guilib/sources/main.c'
      }
    ],
    ...
  ];

=item C<objectname>

C<objectname> actually I<is> a built-in keyword. However, CM Synergy 
B<ccm query -f %objectname> returns the deprecated I<fullname>
(i.e. C<subsystem/cvtype/name/version>) for certain model objects
(e.g. try B<ccm query -f %objectname -i base>) (but refuses to accept
them as arguments later). Therefore C<VCS::CMSynergy> will rewrite
these I<fullname>s to correct I<objectname>s before returning them
from C<query_arrayref> or C<query_hashref>.

=back 

Note the following differences from B<ccm query>:

=over 4

=item *
The keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->query_hashref("name match '*.c'", 
                                    qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    print "$row->{displayname} last modified at $row->{modify_time}\n";
    ...
  }

=item *

These methods do I<not> support any of the
shortcut query options of the B<ccm query> command, e.g.
B<-o owner> or B<-n name>. However, a different shortcut syntax
is supported, see L</"shortcut query notation">.

=back

=head2 query_object, query_object_with_attributes

  $ary_ref = $ccm->query_object($query);
  $ary_ref = $ccm->query_object_with_attributes($query, @attributes);

Executes B<ccm query> with the query expression C<$query> 
and returns a reference to an array of C<VCS::CMSynergy::Object>s 
that satisfy the query.

If there a no hits, a reference to an empty array is returned.  

If there was an error, C<undef> is returned.

Note: This is a convenience method. It might be implemented 
using C<query_arrayref>:

  sub query_object
  {
    my ($self, $query) = @_;
    my $ary = $self->query_arrayref($query, 'object') or return undef;
    [ map { $_->[0] } @$ary ];	# project onto first (and only) column
  }

C<query_object_with_attributes> is only useful when
L</:cached_attributes> is in effect. 
It returns the same result as C<query_object>,
but the returned C<VCS::CMSynergy::Object>s have their attribute caches
primed for the attributes listed in C<@attributes>. You could also
view it as a fancy form of C<query_hashref> where we don't store
the attributes values of C<@attributes> in some anonymous hash,
but rather in the corresponding object. Thus the loop

  for my $obj (@{ $ccm->query_object_with_attributes("...", qw(foo bar)) })
  {
    print "$obj: $obj->{foo} $obj->{bar}\n";
  }

issues a total of I<one> B<ccm> calls. Note: this example assumes 

  use VCS::CMSynergy qw(:cached_attributes :tied_objects);

=head2 shortcut query notation

L<query_arrayref|/"query_arrayref, query_hashref">, 
L<query_hashref|/"query_arrayref, query_hashref">,
L<query_object|/"query_object, query_object_with_attributes"> and
L<query_object_with_attributes|/"query_object, query_object_with_attributes"> 
support a shortcut notation for their common C<$query> parameter. To use
this shortcut, supply a hash reference for C<$query>
(instead of a simple string):

  $result = $ccm->query_hashref(
    { type => 'csrc', match => '*.cpp' }, qw(objectname status));

Every C<< key => value >> represents a simple query. Simple queries
are combined with AND. The following simple queries are accepted:

=over 4

=item "key" => $scalar

This is translated to CMSynergy query syntax as C<key = '$scalar'>.
Note the quotes around C<$scalar>. However, quotes are omitted
if C<$scalar> is either the string C<"TRUE"> or C<"FALSE">. 
In general, C<key> is the name of an attribute. 
The following keys are treated specially:

=over 4

=item match

C<< match => $scalar >> is short for C<name match '$scalar'>.

=item task

C<< task => $tasknr >> is short for 
C<is_associated_cv_of(cvtype = 'task' and task_number = '$tasknr')>.
This corresponds to CM Synergy's B<ccm query -task tasknr>.

=back

=item "key" => \@array

This is translated as a call of a query function, 
i.e. C<key('$array[0]', ...)>. Quoting is as described above.  Example:

  $ccm->query_object(
    { hierarchy_project_members => 
      [ 'toolkit-1.0:project:1', 'none' ] });

=item "key" => \%hash

This is translated as a call of a query function with a nested query
as parameter: Example:

  $rel = '6.0';
  $ccm->query_object(
    { is_member_of => { release => $rel, match => '*web*' });

gets translated to

  "is_member_of(release='6.0' and name match '*web*')" 

=back

=head2 history

  $ary_ref = $ccm->history(@args);

Executes the B<ccm history> command with the given C<@args> as parameters.
The output (probably formatted by the C<-format> option) is split into 
chunks at the divider line (a line consisting of lots of asterisks).
A reference to the resulting array of (multi-line) strings is returned. 

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm history> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.

If you are interested in the successor or predecessor or 
certain attributes of an object in the history,
you should look at the L<history_arrayref|/"history_arrayref, history_hashref">
and L<history_hashref|/"history_arrayref, history_hashref">
methods that return this information in structured form. 

=head2 history_arrayref, history_hashref

  $ary_ref = $ccm->history_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->history_hashref($file_spec, @keywords);

C<history_arrayref> and C<history_hashref>
execute B<ccm history> for C<$file_spec> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. The both return a reference to an array of references,
one per history entry. 

C<history_arrayref> represents a history entry as an array containing
the values of the keywords for that particular object in the history
(in the order given by C<@keywords>). 

C<history_hashref> represents a history entry as a hash
containing attribute and value pairs where the keys are the C<@keywords>.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array or hash element is C<undef> (whereas B<ccm history> would print it as
the string C<< "<void>" >>).

The following names may also be used as keywords though they
are neither built-in nor attributes:

=over 4

=item C<object>

The value is a C<VCS::CMSynergy::Object> representing the object in the history.

=item C<predecessors>

The value returned is a reference to an array of C<VCS::CMSynergy::Object>s
that represent the given object's predecessors.

=item C<successors>

The value returned is a reference to an array of C<VCS::CMSynergy::Object>s
that represent the given object's successors.

=back

Note the following differences from B<ccm history>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=item *

There is no C<-p> (project) option. If you want to get the history
of a project use the full objectname of the project for C<$file_spec>.

=item *

The keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->history_hashref(
    'math.h-1:incl:1', qw(displayname modify_time successors));

  foreach my $row (@$result)
  {
    print "$row->{displayname}: last modified at $row->{modify_time}\n";
    print "\t$_\n" foreach (@{ $row->{successors} });
    ...
  }

=back

=head2 finduse

  $ary_ref = $ccm->finduse(@args);

Executes the B<ccm finduse> command with the given C<@args> as parameters.
It returns a reference to an array of rows, usually one per C<file_spec> given
in C<@args>, or one per query result if C<-query $query_expression>
is present in C<@args>. 

Each row is a reference to an array of two elements.  The first
element is the description of the object.  The second element is a
reference to a hash identifying in what parts of what projects the
object is used.  A key in the hash denotes the project in the form
"project_name-project_version".  The hash value is the corresponding
relative path (including the object's name) in the project.  If there
are no uses of the object in the given scope the hash is empty.  This
usage information is in the same form as that for the pseudo keyword
C<finduse>  of the  L<query_arrayref|/"query_arrayref, query_hashref"> 
and  L<query_hashref|/"query_arrayref, query_hashref"> methods.

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm finduse> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.

If you are interested in usage information for all objects matching a
query you should look at the 
L<query_arrayref|/"query_arrayref, query_hashref"> and
L<query_hashref|/"query_arrayref, query_hashref">
methods, esp. the C<finduse> keyword.

Example (recreate the output of the B<ccm finduse> command):

  foreach (@{ $ccm->finduse(@args) })
  {
    my ($desc, $uses) = @$_;
    print "$desc\n";
    if (keys %$uses)
    {
	while (my ($proj_vers, $path) = each %$uses)
	{
	  print "\t$path\@$proj_vers\n"
	}
    }
    else
    {
	print "\tObject is not used in scope.\n";
    }
  }

=head2 findpath

  $path = $ccm->findpath($file_spec, $proj_vers);

This is a convenience function. It returns the relative pathname
(including the objects's name) for the object C<$file_spec> within the
project C<$proj_vers>.

Returns C<undef> if C<$file_spec> is not used in C<$proj_vers>
or if C<$file_spec> does not exist.

Example:

  $ccm->findpath("main.c-1:csrc:3", "guilib-darcy"); 

returns 

  "guilib/sources/main.c"

=head2 traverse_project

  traverse_project(\&wanted, $project, $dir);
  traverse_project(\%options, $project, $dir);

C<traverse_project> walks the tree below directory C<$dir>
in project C<$project> without the need for a workarea. 
It is modelled on L<File::Find>.

C<&wanted> is a code reference described in 
L</"wanted function"> below.
C<$project> can be any project specification (in project-version form,
an objectname or a C<VCS::CMSynergy::Object>). However, C<$dir>
must be a C<VCS::CMSynergy::Object>. If C<$dir> is omitted,
it defaults to the top level directory of C<$project>.

=head3 wanted function

C<&wanted> is called once for all objects below C<$dir> 
including C<$dir> itself. It will also be called on subprojects
of C<$project>, but C<traverse_project> will not recurse into
subprojects unless the C<subprojects> flag is specified 
(see L</"options"> below).

On each call to C<&wanted>, C<$_> will be bound to the 
currently traversed object (a C<VCS::CMSynergy::Object>). 

C<@VCS::CMSynergy::Traversal::dirs> will be bound to 
an array of C<VCS::CMSynergy::Object>s of cvtype C<dir> representing 
the path in C<$project> project from C<$dir> to C<$_>.
In particular, C<@VCS::CMSynergy::Traversal::dirs[-1]>
is the parent C<dir> of C<$_>.

Similarly C<@VCS::CMSynergy::Traversal::projects> represents the
subproject hierarchy starting with
C<$project>. In particular, C<$_> is a member of 
C<$VCS::CMSynergy::Traversal::projects[-1]>.

You may set C<$VCS::CMSynergy::Traversal::prune> to a true
value in C<&wanted> to stop recursion into sub directories (or subprojects)
(this makes only sense when C<&wanted> is called 
on a C<dir> or C<project> object).

If recursion into subprojects is specfied, C<&wanted>
will be called once for the C<project> object and also for the
top level C<dir> of the subproject.

=head3 options

The first argument of C<traverse_project> may also be a hash reference.
The following keys are supported:

=over 4

=item C<wanted> (code reference)

The value should be a code reference. It is described in
L</"wanted function">.

=item C<bydepth> (boolean)

If this option is set, C<traverse_project>
calls C<&wanted> on a directory (or project) only B<after> 
all its entries have been processed. It is "off" by default.

=item C<preprocess> (code reference)

The value should be a code reference. It is used to preprocess
the children of a C<dir> or C<project>, i.e. B<before> L<traverse_project>
starts traversing it. You can use it to impose an ordering
among "siblings" in the traversal. You can also filter out
objects, so that C<wanted> will never be called on them
(and traversal will not recurse on them in case of
C<dir>s or C<project>s).

The preprocessing function is called with
a list of C<VCS::CMSynergy::Object>s and is expected to return
a possibly reordered subset of this list. Note that
the list may contain C<dir> and C<project> objects.
When the preprocessing function is called,
C<$_> is bound to the parent object (which is always
of C<cvtype> C<dir> or C<project>).

=item C<postprocess> (code reference)

The value should be a code reference. It is invoked just before
leaving the current C<dir> or C<project>.

When the postprocessing function is called,
C<$_> is bound to the current object  (which is always
of C<cvtype> C<dir> or C<project>).

=item C<subprojects> (boolean)

If this option is set, C<traverse_project>
will recurse into subprojects. It is "off" by default.

=item C<attributes> (array ref)

This option is only useful if L</:cached_attributes> is in effect. 
It should contain a reference to an
array of attribute names. If present, C<traverse_project>
uses C<query_object_with_attributes> rather than
C<query_object> for the traversal. Hence all objects encountered
in the traversal (e.g. C<$_> when bound in C<wanted> or the elements
of the directory stack C<@VCS::CMSynergy::Traversal::dirs>) have
their attribute caches primed for the given attributes,
cf. L<query_object_with_attributes|/"query_object, query_object_with_attributes">.

=back

Note that for any particular C<dir> (or C<project>) object,
the above code references are always called in order
C<preprocess>, C<wanted>, C<postprocess>.

Example: 

  $ccm->traverse_project(
    sub {
      print join("/", 
        map { $_->name } @VCS::CMSynergy::Traversal::dirs, $_), "\n"
	  unless $_->cvtype eq 'project'; 
    },
    'toolkit-1.0:project:1');

This prints the directory tree of project B<toolkit-1.0:project:1>
similar to the Unix command L<find>. The order of entries in a directory
is unspecified and sub projects are not traversed:

  toolkit
  toolkit/makefile
  toolkit/makefile.pc
  toolkit/misc
  toolkit/misc/toolkit.ini
  toolkit/misc/readme

Another example:

  $ccm->traverse_project(
    {
      wanted => sub {
	return unless $_->cvtype eq "project";
	my $proj_depth = @VCS::CMSynergy::Traversal::projects;
	print "  " x $proj_depth, $_->displayname, "\n";
      },
      preprocess => sub { sort { $a->name cmp $b->name } @_; },
      subprojects => 1,
    },
    "toolkit-1.0:project:1");

This prints the complete project hierarchy rooted at  
B<toolkit-1.0:project:1>.  Only projects will be shown,
entries are sorted by name and are intended according to their depth:

  toolkit-1.0
    calculator-1.0
    editor-1.0
    guilib-1.0

=head1 ATTRIBUTE METHODS

=head2 get_attribute

  $value = $ccm->get_attribute($attr_name, $file_spec);

Get the value of the attribute C<$attr_name> for
C<$file_spec> (using B<ccm attribute -show>). 

If C<RaiseError> is not
set and an error occurs (e.g.  attribute C<$attr_name> does not exist
on object C<$file_spec>), C<undef> will be returned.

Note the following differences from B<ccm attribute -show>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=item *

There is no C<-p> (project) option. If you want to get an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 set_attribute

  $ccm->set_attribute($attr_name, $file_spec, $value);

Set the value of the attribute C<$attr_name>
for C<$file_spec> to C<$value> (using (B<ccm attribute -modify>).

Returns C<$value> on success.  If C<RaiseError>
is not set and an error occurs (e.g. attribute C<$attr_name> does not
exist on object C<$file_spec>), C<undef> will be returned.

This works for B<all> types of attributes, even those of type I<text>
(or derived from I<text>) and with C<$value>s that may contain
multiple lines or are of arbitrary length.

Note the following differences from B<ccm attribute -modify>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 create_attribute

  $ccm->create_attribute($attr_name, $type, $value, @file_specs);

Create attribute C<$attr_name> of type C<$type> on all objects
given by C<@file_specs> (using B<ccm attribute -create>).
You may also set an initial value
by passing something other than C<undef> for C<$value>.

Note the following differences from B<ccm attribute -create>:

=over 4

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 delete_attribute

  $ccm->delete_attribute($attr_name, @file_specs);

Delete attribute C<$attr_name> from all objects
given by C<@file_specs> (using B<ccm attribute -delete>).


Note the following differences from B<ccm attribute -create>:

=over 4

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 copy_attribute

  $ccm->copy_attribute($attr_name, $flags, $from_file_spec, @to_file_specs);

Copy attribute C<$attr_name> from C<$from_file_spec>
by objects given by C<@to_file_specs> (using B<ccm attribute -copy>).

You can specify multiple attributes to copy by passing
a reference to an array of attribute names as C<$attr_name>.

C<$flags> may be C<undef> or a reference to an array containing
a subset of the following strings: C<"append">, C<"subproj">,
C<"suball">, e.g.

  $ccm->copy_attribute($attr_name, [ qw(subproj suball) ], 
  	               "proja-1.0:project:1", "projb-1.0:project:1");

Cf. the CM Synergy documentation on the I<attribute command>
for the meaning of these flags.

Note the following differences from B<ccm attribute -create>:

=over 4

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 list_attributes

  $hash_ref = $ccm->list_attributes($file_spec);

Lists all attributes for C<$file_spec> (using B<ccm attribute -la>).

Returns a reference to a hash containing pairs of attribute name
and attribute type (e.g. C<string>, C<time>).
Returns C<undef> in case of error.

Note the following differences from B<ccm attribute -la>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=back

=head2 property

  $value = $ccm->property($keyword, $file_spec);

Returns the value of property C<$keyword> for C<$file_spec>
(using B<ccm properties -f ...>).
You can use any of the CM Synergy built-in keywords for C<$keyword>.
If the value of C<$keyword> is undefined, C<undef> is returned
(whereas B<ccm properties> would print it as the string C<< "<void>" >>).

=head1 MISCELLANEOUS METHODS

=head2 ls

  $ary_ref = $ccm->ls(@args);

Executes the B<ccm ls> command with the given C<@args> as parameters.
The output (as formatted by the C<-format> option) is split into lines.
These are L<chomp|perlfunc/chomp>ed and a reference to the resulting array
of strings is returned. 

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm ls> argument or option as a single
Perl argument. 

If you are interested to obtain the value of several attributes,
you should look at the L</ls_arrayref>
and L</ls_hashref> methods that return this information in 
structured form. If you are only interested in the identity of
the listed objects, you should look at the L</ls_object> method.

=head2 ls_object

  $ary_ref = $ccm->ls_object($file_spec);

Lists information about a file or the contents of a directory
using the work area name C<$file_spec>.
Returns a reference to an array of corresponding C<VCS::CMSynergy::Object>s.
The default C<$file_spec> is the working directory.

=head2 ls_arrayref

  $ary_ref = $ccm->ls_arrayref($file_spec, @keywords);

Lists the values of the built-in keywords or attributes supplied
in C<@keywords> for a file or the contents of a directory
Returns a reference to an array of references,
one per result row. Each reference points to an array containing
the values of the keywords for that particular object
(in the order given by C<@keywords>). 

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array element is C<undef> (whereas B<ccm ls> would print it as
the string C<< "<void>" >>).

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->ls('foo', qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    my ($displayname, $type, $modify_time) = @$row;
    print "$displayname ($type) last modified at $modify_time\n";
    ...
  }

=head2 ls_hashref

  $ary_ref = $ccm->ls_hashref($file_spec, @keywords);

Lists the values of the built-in keywords or attributes supplied
in C<@keywords> for a file or the contents of a directory
using the work area name C<$file_spec>.
Returns a reference to an array of references,
one per result row. Each reference points to hash containing
attribute and value pairs where the keys are C<@keywords>.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
hash element is C<undef> (whereas B<ccm ls> would print it as
the string C<< "<void>" >>).

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->ls_hashref('foo', qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    print "$row->{displayname} last modified at $row->{modify_time}\n";
    ...
  }

=head2 set

  $value = $ccm->set($option);
  $old_value = $ccm->set($option, $new_value);
  $hash_ref = $ccm->set;

Get or set the value of an option.

In the first form, C<set> returns the value of C<$option>. 
If the option is unset, C<undef> is returned 
(whereas B<ccm set> would print C<"(unset)"> in this case).

In the second form, the C<$option> is set to C<$new_value>, the previous
value is returned. If C<$new_value> is C<undef>, C<$option> is unset.

In the third form, a reference to a hash is returned. The hash consists
of all currently defined options as keys and their respective values.

=head2 ccm_with_option, ccm_with_text_editor

  ($rc, $out, $err) = $ccm->ccm_with_option($option, $value, @cmd);
  ($rc, $out, $err) = $ccm->ccm_with_text_editor($text_value, @cmd);

These are convenience functions for executing a command 
that is sensitive to the value of a session option, esp. B<text_editor>.

C<ccm_with_option> does the following:

=over 4

=item *

saves the old value of C<$option> and sets it to C<$value> with

  $ccm->set($option, $value);

=item *

with the new setting in effect, executes

  $ccm->ccm(@cmd);

=item *

finally restores the old value of C<$option>.

=back

C<ccm_with_text_editor> is useful in scripting B<ccm> commands
like B<ccm users>. These commands usually open a temporary file
generated by CM Synergy in a user-specified editor. Then the user edits 
the contents and save her changes. Finally, CM Synergy reads back 
the temporary file and does something with the (changed) contents. 

C<ccm_with_text_editor> does the following

=over 4

=item *

creates a temporary file (using L<File::Temp::tempfile>), 
say C</tmp/a5Xghd>, and writes the string C<$text_value> into it,

=item *

then executes (on Unix)

  $ccm->ccm_with_value(
    text_editor => "cp /tmp/a5Xghd %filename", @cmd);

which causes CM Synergy to accept C<$text_value> as the "updated value"
w.r.t. to command C<@cmd>,

=item *

finally removes the temporary file.

=back

Bot C<ccm_with_option> and C<ccm_with_text_editor> return the same
value as the inner L</ccm> method, except when there is an error setting
the new C<$value> of C<$option>.

=head2 get_releases, set_releases

  $releases = $ccm->get_releases;
  $ccm->set_releases($releases);

C<get_releases> fetches the release table (of active releases) as printed
by B<ccm releases -show>. It returns a reference to a hash where
each  key is the release name and the value is (a reference to) 
a list of included releases, e.g. as formatted by L<Data::Dumper>:

  $releases = {
      '1.0'	=> [ qw(1.0) ],
      '1.1'	=> [ qw(1.0 1.1) ],
      '2.0'	=> [ qw(1.0 1.1 2.0) ],
      '2.0_SP1'	=> [ qw(1.0 1.1 2.0 2.0_SP1) ],
      '2.1'	=> [ qw(1.0 1.1 2.0 2.1) ],
      '3.0'	=> [ qw(1.0 1.1 2.0 2.1 3.0) ],
      '3.1'	=> [ qw(1.0 1.1 2.0 2.1 3.0 3.1) ]
  };

C<set_releases> updates the release table. It takes a reference to
a hash with the same structure as returned by C<get_releases>.

Note: This methods I<do not work> on CM Synergy 6.3 and higher,
because the B<releases> command has been superceded by the 
incompatible, though more powerful, B<release> command.

=head2 ccm_addr

  print "CCM_ADDR=", $ccm->ccm_addr;

Returns the session's RFC address.

=head2 database

  $database = $ccm->database;

Returns the database path in canonical form (i.e. with a trailing C<"/db">):

=head2 delimiter

  $delim = $ccm->delimiter;

Returns the database delimiter.

=head2 ping

  if ($ccm->ping) { ... }

C<ping> tests whether session C<$ccm> is still alive 
(without causing an exception if it fails).

This could be used e.g. from a web application that keeps a pool
of established CM Synergy sessions to deal with user requests:
before invoking a command on a session the application must make
sure that the session is still valid. If not, it will automatically
create a new session.

=head2 object

  $obj1 = $ccm->object($objectname);
  $obj2 = $ccm->object($name, $version, $cvtype, $instance);

Create a C<VCS::CMSynergy::Object> from either an I<objectname>
(sometimes called "object reference form" in CM Synergy documentation)
in "name-version:cvtype:instance" format or the four parts specified
separately. 

This is just a wrapper for L<VCS::CMSynergy::Object/new>.
However, C<new> requires the four parts of the I<objectname>
to be specified as separate arguments.

Note that no check is made whether the specified object really exists
in the database.

=head1 METHODS INHERITED FROM C<VCS::CMSynergy::Client>

C<VCS::CMSynergy> is derived from L<VCS::CMSynergy::Client>,
hence the following methods are inherited from the latter:

=over 4

=item C<ccm_home>

=item C<error>, C<set_error>

=item C<ccm_command>, C<out>, C<err>

=item C<trace>, C<trace_msg>

=item C<version>

=item C<ps>

=item C<status>

=back

Note: All these methods can be invoked on a session object
or as class methods.

=head1 SEE ALSO

L<VCS::CMSynergy::Object>, 
L<VCS::CMSynergy::Client>,
L<VCS::CMSynergy::Users> 

=head1 AUTHORS

Roderich Schupp, argumentum GmbH <schupp@argumentum.de>

=head1 COPYRIGHT AND LICENSE

The VCS::CMSynergy module is Copyright (c) 2001-2004 argumentum GmbH, 
L<http://www.argumentum.de>.  All rights reserved.

You may distribute it under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=cut
