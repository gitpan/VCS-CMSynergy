package VCS::CMSynergy::Helper;

our $VERSION = sprintf("%d.%02d", q%version: 1.21 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy::Helper - ancillary convenience functions

=head1 SYNOPSIS

  my %ccm_opts = VCS::CMSynergy::Helper::GetOptions;

=head2 GetOptions

This function extracts a set of common options from C<@ARGV> and converts
them to the corresponding options for L<VCS::CMSynergy/new>.
All options and arguments in C<@ARGV> that it doesn't know about 
are left untouched. 

It may be used to make all your CM Synergy scripts accept a
uniform set of options:

  use Getopt::Long;
  use VCS::CMSynergy;
  use VCS::CMSynergy::Helper;
  ...

  # extract CM Synergy options from @ARGV
  my %ccm_opts = VCS::CMSynergy::Helper::GetOptions;

  # process other options in @ARGV
  GetOptions(...);

  # start CM Synergy session
  my $ccm = VCS::CMSynergy->new(
      %ccm_opts,
      RaiseError => 1,
      PrintError => 0);
  ...

The following options are recognized:

=over 4

=item C<-D>, C<--database>

absolute database path; this option corresponds to option C<database>
for C<VCS::CMSynergy/start>

=item C<-H>, C<--host>

engine host; this option corresponds to option C<host> for
for C<VCS::CMSynergy/start>

=item C<-U>, C<--user>

user; this option corresponds to option C<user>
for C<VCS::CMSynergy/start>

=item C<-P>, C<--password>

user's password; this option corresponds to option C<password> for
for C<VCS::CMSynergy/start>

=item C<--ui_database_dir>

path name to which your database information is copied when you are 
running a remote client session; this option corresponds to 
option C<ui_database_dir> for B<ccm start>, it implies C<remote_client>

=back

If no database was specified C<GetOptions> adds 

  CCM_ADDR => $ENV{CCM_ADDR}

to the returned hash; it will L<croak|Carp/croak> if $ENV{CCM_ADDR}
is not defined.

Note that all recognized single letter options are in uppercase so that
scripts using C<VCS::CMSynergy::Helper::GetOptions> still
can use all lowercase letters for their own options.

Here's the short description of recognized options that you 
can cut and paste into your script's POD:

  CM Synergy Options:

  -D PATH | --database PATH       database path
  -H HOST | --host HOST           engine host
  -U NAME | --user NAME           user name
  -P STRING | --password STRING   user's password
  --ui_database_dir PATH          path to copy database information to

=cut

require Getopt::Long;
use Carp;

sub GetOptions()
{
    my %opts;
    
    Getopt::Long::Configure(qw(no_ignore_case passthrough));

    Getopt::Long::GetOptions(\%opts,
	'database|D=s',
	'host|H=s',
	'user|U=s',
	'password|P=s',
	'ui_database_dir=s');
    $opts{remote_client} = 1 if exists $opts{ui_database_dir};

    Getopt::Long::Configure(qw(no_passthrough));

    unless (defined $opts{database})
    {
	# default CCM_ADDR from environment if no --database was specified;
	# croak if neither was specified
	$opts{CCM_ADDR} = $ENV{CCM_ADDR} or
	    croak "Don't know how to connect to CM Synergy:\nneither CCM_ADDR set in environment, nor database specified in options (with -D or --database)\n";
    }

    return %opts;
}

1;
