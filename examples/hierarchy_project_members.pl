#!/opt/gnu/bin/perl

# List all sub projects of a given project

# Usage: hierarchy_project_members.pl database top_proj_vers

use VCS::CMSynergy; 
use strict;

die "usage: $0 database top_proj_vers" unless @ARGV == 2;
my ($database, $top_proj_vers) = @ARGV;

my $ccm = VCS::CMSynergy->new(
    database	=> $database,
    RaiseError	=> 1
);

foreach my $member (@{ $ccm->hierarchy_project_members(
    $top_proj_vers, 'depth', qw(object status wa_path)) })
{
    print "$member->{object} <$member->{status}> $member->{wa_path}\n";
}

=item C<hierarchy_project_members>

  $ary_ref = $ccm->hierarchy_project_members(
    $proj_vers, $order_spec, @keywords);

Convenience function. Uses the built-in CM Synergy query function of the
same name with arguments C<$proj_vers> and C<$order_spec>.

If C<@keywords> are omitted, C<hierarchy_project_members> invokes
L</query_object>, i.e. returns a reference to an array of 
C<VCS::CMSynergy::Object>s. Otherwise C<hierarchy_project_members> 
invokes L</query_hashref> where
C<@keywords> are simply passed down to L</query_hashref>.
In the latter case it return a reference to an array of hash refs.

If <$order_spec> is not given, C<'none'> is assumed.

NOTE: While the CM Synergy function B<hierarchy_project_members> accepts
only objectname as its first argument, this method
accepts a VCS::CMSynergy::Object, an objectname, or a proj_vers.

=cut

package VCS::CMSynergy;

sub hierarchy_project_members
{
    my ($self, $proj_vers, $order_spec, @keywords) = @_;

    $proj_vers .= ":project:1" 
	unless ref $proj_vers || $proj_vers =~ /:project:/;
    $order_spec = 'none' unless defined $order_spec;

    my $query = "hierarchy_project_members('$proj_vers', '$order_spec')";
    return @keywords ? 
	$self->query_hashref($query, @keywords) :
	$self->query_object($query);
}

