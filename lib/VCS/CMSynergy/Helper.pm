package VCS::CMSynergy::Helper;

require Getopt::Long;
use Carp;

=head1 SYNOPSIS

  CM Synergy Options:

  -D PATH | --database PATH       database path
  -H HOST | --host HOST           engine host
  -U NAME | --user NAME           user name
  -P STRING | --password STRING   user's password
  --ui_database_dir PATH          path to copy database information to

=head1 C<GetOptions>

  use Getopt::Long;
  use VCS::CMSynergy;
  use VCS::CMSynergy::Helper;
  ...

  # extract CM Synergy options from @ARGV
  my @ccm_opts = VCS::CMSynergy::Helper::GetOptions;

  # process other options in @ARGV
  GetOptions(...);

  # start CM Synergy session
  my $ccm = VCS::CMSynergy->new(
      @ccm_opts,
      RaiseError => 1,
      PrintError => 0);

All single letter options are in uppercase so that
scripts using C<VCS::CMSynergy::Helper::GetOptions> still
can use all lowercase letters for their own options.

=over 4

=item c<-D>, C<--database>

absolute database path; this option corresponds to option C<database>
for C<VCS::CMSynergy/start>

=item c<-H>, C<--host>

engine host; this option corresponds to option C<host> for
for C<VCS::CMSynergy/start>

=item c<-U>, C<--user>

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

to the returned hash. It will L<carp|Carp/carp> if $ENV{CCM_ADDR}
is not defined.

=cut

sub GetOptions()
{
    my %opts;
    
    Getopt::Long::Configure('passthrough');

    Getopt::Long::GetOptions(\%opts,
	'database|D=s',
	'host|H=s',
	'user|U=s',
	'password|P=s',
	'ui_database_dir=s');
    $opts{remote_client} = 1 if exists $opts{ui_database_dir};

    Getopt::Long::Configure('no_passthrough');

    unless (defined $opts{database})
    {
	unless (defined ($opts{CCM_ADDR} = $ENV{CCM_ADDR}))
	{
	    carp("no database specified and CCM_ADDR not set");
	    return;
	}
    }

    return %opts;
}

1;
