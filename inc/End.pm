#line 1
package End;

#
# $Id: End.pm,v 1.2 2000/05/31 20:25:33 abigail Exp $
#
# $Log: End.pm,v $
# Revision 1.2  2000/05/31 20:25:33  abigail
# Added the license paragraph in the POD section.
#
# Revision 1.1  2000/05/31 19:35:01  abigail
# Initial revision
#

use strict;

use Exporter;
use vars qw /@EXPORT @ISA $VERSION/;

@ISA    = qw /Exporter/;
@EXPORT = qw /end/;

($VERSION) = '$Revision: 1.2 $' =~ /([\d.]+)/;

sub end (&) {
    my    $code =  shift;
    # Due to a bug in Perl 5.6.0, we can't just bless $code.
    # But by creating an extra closure, it'll work.
    bless sub {$code -> ()} => __PACKAGE__;
}

DESTROY {$_ [0] -> ()}

1;

__END__

#line 120
