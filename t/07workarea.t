#!/usr/bin/perl -w

use Test::More tests => 17;
use t::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

use Cwd;
use File::Path;
use File::Spec;
use File::Temp qw(tempdir);

BEGIN
{
    if ($^O eq 'cygwin')
    { 
	require Filesys::CygwinPaths; import Filesys::CygwinPaths qw(:all);
    }
}

my @cleanup;			# cleanup actions
END { &{ pop @cleanup } while @cleanup; }

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $pname = "calculator";
my $pversion = "test$$";
my $tempdir = tempdir(CLEANUP => 1);
my $pwd = getcwd;
my $result = $ccm->query_object(
    qq[type='project' and name='$pname' and version match 'test*']);
ok(@$result == 0, qq[test project ${pname}-test* does not exist yet]);

ok($ccm->checkout(-project => "${pname}-1.0", "-copy_based", 
                  -to => $pversion, -path => $tempdir),
    qq[checkout project ${pname}-1.0 to $pversion]);
$result = $ccm->query_object({ type => 'project', name => $pname, version => $pversion });
ok(@$result == 1, qq[test project ${pname}-${pversion} has been created]);

my ($proj) = @$result;
my $wa_path = $ccm->get_attribute(wa_path => $proj);
ok(index($wa_path, $tempdir) == 0, qq[wa_path "$wa_path" is below checkout path "$tempdir"]);
push @cleanup, sub
{
    chdir($pwd);
    ok($ccm->delete(-project => $proj), qq[test project $proj has been deleted]);
    ok(! -d $wa_path, q[test project workarea has been deleted]);
};

my $ccmwaid = File::Spec->catfile(
    $wa_path, $pname,
    VCS::CMSynergy::Client::is_win32 ? "_ccmwaid.inf" : ".ccmwaid.inf");
ok(-e $ccmwaid, qq[check for ccmwaid file ($ccmwaid) below wa_path]);

# chdir to project sub directory (esp. for testing coprocess)
ok(chdir(File::Spec->catdir($wa_path, $pname, "sources")), 
   q[chdir to project sub directory "sources"]);

my $file = "clear.c";
ok(-e $file, qq[file $file exists]);
ok(! -w $file, qq[file $file is read-only]);

# use workarea name to specify an object
is($ccm->get_attribute(status => $file), "released",
   q[get_attribute via workarea name]);

# check out an object
ok($ccm->checkout($file), q[check out $file]);
push @cleanup, sub
{
    ok($ccm->delete(-replace => $file), qq[delete and replace $file]);
};
ok(-w $file, qq[file $file is now writable]);
is($ccm->get_attribute(status => $file), "working", 
    q[checked out file is "working"]);

exit 0;

