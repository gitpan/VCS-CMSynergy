#!/usr/bin/perl -w

use Test::More tests => 11;
use Test::Deep 0.093;
use t::util;
use strict;

BEGIN 
{ 
    my @use = ();
    push @use, ':cached_attributes' if $ENV{CCM_USE_CACHED_ATTRIBUTES};
    use_ok('VCS::CMSynergy', @use); 
    SKIP:
    {
	skip "not using :cached_attributes", 1 
	    unless $ENV{CCM_USE_CACHED_ATTRIBUTES};
	ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
    }
}

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};
diag("using :cached_attributes") if VCS::CMSynergy::use_cached_attributes();

# if Synergy version is 6.4 check for SP1 and higher
# (full version 6.4.nnnn with nnnn >= 3410)
my $micro_version = (split(/\./, ($ccm->version)[0]))[2];

my $from_exp = [
    {
	to => {
	    objectname => 'calculator-1.0:project:1',
	    status_log => "Wed Aug 13 15:26:09 1997: Status set to 'working' by ccm_root in role ccm_admin\nWed Aug 13 16:29:00 1997: Status set to 'released' by ccm_root in role ccm_admin",
	    task_number => undef
	},
	from => { objectname => 'calculator-int:project:1' },
	name => 'baseline_project',
	create_time => ignore(),
    },
    {
	to => {
	    objectname => '4-1:folder:probtrac',
	    status_log => "Mon Nov 25 17:56:37 2002: Status set to 'working_folder' by ccm_root in role ccm_admin\nMon Nov 25 17:56:38 2002: Status set to 'prep_folder' by ccm_root in role ccm_admin",
	    task_number => undef
	},
	from => { objectname => 'calculator-int:project:1' },
	name => 'folder_in_rp',
	create_time => ignore(),
    },
    {
	to => {
	    objectname => 'Toolkit%002f2.0%003aintegrate-1:recon_temp:1',
	    status_log => "Mon Dec 16 16:46:17 2002: Status set to 'working_recon_temp' by steveh in role ccm_admin",
	    task_number => undef
	},
	from => { objectname => 'calculator-int:project:1' },
	name => 'reconfigure_template',
	create_time => ignore(),
    },
    {
	to => {
	    objectname => 'task37-1:task:probtrac',
	    status_log => "Mon Jun 16 14:13:46 2003: Status set to 'registered' by ccm_root in role ccm_admin\nMon Jun 16 14:13:46 2003: Status set to 'task_automatic' by ccm_root in role ccm_admin",
	    task_number => '37'
	},
	from => { objectname => 'calculator-int:project:1' },
	name => 'task_in_rp',
	create_time => ignore(),
    },
    {
	to => {
	    objectname => 'task38-1:task:probtrac',
	    status_log => "Mon Jun 16 14:13:46 2003: Status set to 'registered' by ccm_root in role ccm_admin\nMon Jun 16 14:13:46 2003: Status set to 'task_automatic' by ccm_root in role ccm_admin",
	    task_number => '38'
	},
	from => { objectname => 'calculator-int:project:1' },
	name => 'task_in_rp',
	create_time => ignore(),
    }
];

push @$from_exp,
    {
	to => {
	      objectname => 'Toolkit%002f2.0%003aintegrate-1:project_grouping:1',
	      status_log => "Thu Mar 10 10:32:53 2005: Status set to 'working' by ccm_root\nThu Mar 10 10:32:53 2005: Status set to 'prep' by ccm_root",
	      task_number => undef
	},
	from => { objectname => 'calculator-int:project:1' },
	name => 'project_grouping',
	create_time => ignore(),
    }
    if $ccm->version == 6.4 && $micro_version >= 3410;	# 6.4 SP1 and higher
	
my $from_got = $ccm->relations_hashref(
    from		=> "calculator-int:project:1",
    from_attributes	=> [ qw/ objectname / ],
    to_attributes	=> [ qw/ objectname status_log task_number / ]);
verbose('from_got', $from_got);

cmp_bag($from_got, $from_exp, "any relations FROM project");
cmp_deeply($from_got, array_each(
    superhashof({ from => shallow($from_got->[0]->{from}) })), 
    "FROM values are the same hash");


my $to_exp = [
    {
	to => { objectname => 'calculator-int:project:1' },
	from => {
	    objectname => 'task37-1:task:probtrac',
	    status_log => "Mon Jun 16 14:13:46 2003: Status set to 'registered' by ccm_root in role ccm_admin\nMon Jun 16 14:13:46 2003: Status set to 'task_automatic' by ccm_root in role ccm_admin",
	    task_number => '37'
	},
	name => 'associated_cv',
	create_time => ignore(),
    },
    {
	to => { objectname => 'calculator-int:project:1' },
	from => {
	    objectname => 'calculator-int_20021125:project:1',
	    status_log => "Mon Nov 25 18:36:31 2002: Status set to 'working' by ccm_root in role build_mgr\nMon Nov 25 18:36:32 2002: Status set to 'prep' by ccm_root in role build_mgr\nMon Nov 25 18:36:39 2002: Status set to 'integrate' by ccm_root in role build_mgr",
	    task_number => undef
	},
	name => 'successor',
	create_time => ignore(),
    }
];
push @$to_exp, 
    {
	to => { objectname => 'calculator-int:project:1' },
	from => {
	    objectname => 'Toolkit%002f2.0%003aintegrate-1:project_grouping:1',
	    status_log => "Thu Mar 10 10:32:53 2005: Status set to 'working' by ccm_root\nThu Mar 10 10:32:53 2005: Status set to 'prep' by ccm_root",
	    task_number => undef
	},
	name => 'project_in_pg',
	create_time => ignore(),
    } 
    if $ccm->version == 6.4 && $micro_version < 3410;	# 6.4 before SP1

my $to_got = $ccm->relations_hashref(
    to			=> "calculator-int:project:1",
    to_attributes	=> [ qw/ objectname / ],
    from_attributes	=> [ qw/ objectname status_log task_number / ]);
verbose('to_got', $to_got);
cmp_bag($to_got, $to_exp, "any relations TO project");
cmp_deeply($to_got, array_each(
    superhashof({ to => shallow($to_got->[0]->{to}) })), 
    "TO values are the same hash");


my $to_name_got = $ccm->relations_hashref(
    to			=> "calculator-int:project:1",
    name		=> "successor",
    to_attributes	=> [ qw/ objectname / ],
    from_attributes	=> [ qw/ objectname status_log task_number / ]);
verbose('to_name_got', $to_name_got);
cmp_bag($to_name_got, [ grep { $_->{name} eq "successor" } @$to_exp ],
    "SUCCESSOR relations TO project");


my $name_exp = [
    {
      to => { objectname => 'task35-1:task:probtrac' },
      from => { objectname => 'toolkit-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task36-1:task:probtrac' },
      from => { objectname => 'toolkit-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'toolkit-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'toolkit-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'editor-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'editor-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task35-1:task:probtrac' },
      from => { objectname => 'editor-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task36-1:task:probtrac' },
      from => { objectname => 'editor-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task35-1:task:probtrac' },
      from => { objectname => 'guilib-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task36-1:task:probtrac' },
      from => { objectname => 'guilib-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task35-1:task:probtrac' },
      from => { objectname => 'calculator-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task36-1:task:probtrac' },
      from => { objectname => 'calculator-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'guilib-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'guilib-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'calculator-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'calculator-int:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task39-1:task:probtrac' },
      from => { objectname => 'toolkit-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task40-1:task:probtrac' },
      from => { objectname => 'toolkit-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task39-1:task:probtrac' },
      from => { objectname => 'editor-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task33-1:task:probtrac' },
      from => { objectname => 'sandbox-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task34-1:task:probtrac' },
      from => { objectname => 'sandbox-1.0:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task40-1:task:probtrac' },
      from => { objectname => 'editor-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task39-1:task:probtrac' },
      from => { objectname => 'guilib-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task40-1:task:probtrac' },
      from => { objectname => 'guilib-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task39-1:task:probtrac' },
      from => { objectname => 'calculator-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task40-1:task:probtrac' },
      from => { objectname => 'calculator-darcy:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'calculator-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'calculator-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'editor-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'editor-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'guilib-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'guilib-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task37-1:task:probtrac' },
      from => { objectname => 'toolkit-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    },
    {
      to => { objectname => 'task38-1:task:probtrac' },
      from => { objectname => 'toolkit-int_20021125:project:1' },
      create_time => ignore(),
      name => 'task_in_rp',
    }
];

my $name_got = $ccm->relations_hashref(
    name		=> "task_in_rp",
    to_attributes	=> [ qw/ objectname / ],
    from_attributes	=> [ qw/ objectname / ]);
verbose('name_got', $name_got);
cmp_bag($name_got, $name_exp, "all TASK_IN_RP relations");


my $empty_got = $ccm->relations_hashref(
    to			=> "bufcolor.c-1:csrc:1",
    to_attributes	=> [ qw/ objectname / ],
    from_attributes	=> [ qw/ objectname status_log task_number / ]);
verbose('empty_got', $empty_got);
cmp_bag($empty_got, [], "empty set of TO relations");


my $frobozz = eval { $ccm->relations_hashref(to => "frobozz-42:csrc:1"); };
ok($@, "non-existing TO object throws exception");

exit 0;