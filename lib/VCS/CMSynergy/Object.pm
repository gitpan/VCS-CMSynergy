package VCS::CMSynergy::Object;

# %version: 1.17 %

=head1 NAME

VCS::CMSynergy::Object - Convenience wrapper to treat objectnames as an object

=head1 SYNOPSIS

  use VCS::CMSynergy;
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  $obj1 = $ccm->object($name, $version, $cvtype, $instance);
  $obj2 = $ccm->object($objectname);

  # objectname and its constituents
  print "...and the object is $obj";
  print "name       = ", $obj->name;
  print "version    = ", $obj->version;
  print "cvtype     = ", $obj->cvtype;
  print "instance   = ", $obj->instance;
  print "objectname = ", $obj1->objectname;

  # attribute methods, optionally caching with 
  #   use VCS::CMSynergy ':cached_attributes'
  print $obj->get_attribute('comment');
  $obj->set_attribute(comment => "blurfl");
  $obj->create_attribute(foo => "some text");
  $obj->delete_attribute("foo");
  $hashref = $obj->list_attributes;	# always caches result

  # property methods
  print $obj->property("bar");
  print $obj->displayname;		# always caches result

  ## tiehash interface
  use VCS::CMSynergy ':tied_objects';
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  print $obj->{comment};	
  $obj->{comment} = "blurfl";	
  # same as:
  #   print $ccm->get_attribute(comment => $obj);
  #   $ccm->set_attribute(comment => $obj, "blurfl");


This synopsis only lists the major methods.

=cut 

    
use Carp;

use overload 
    '""'	=> \&objectname,
    cmp		=> sub { $_[0]->objectname cmp $_[1]->objectname },
    fallback	=> 1;

{
    # generate getter methods
    no strict 'refs';
    foreach my $method (qw(objectname ccm name version cvtype instance))
    {
	*{$method} = sub { shift->{$method}; };
    }
}

my $have_weaken = eval "use Scalar::Util qw(weaken); 1";


# VCS::CMSynergy::Object->new(ccm, name, version, cvtype, instance)
sub new
{
    unless (@_ == 6)
    {
	carp(__PACKAGE__ . " new: illegal number of arguments");
	return undef;
    }
    my $class = shift;

    my %hash;
    @hash{qw(ccm name version cvtype instance)} = @_;
    $hash{objectname} = $_[1] . $_[0]->delimiter . "$_[2]:$_[3]:$_[4]";
    Scalar::Util::weaken($hash{ccm}) if $have_weaken;
    $hash{acache} = {} if $VCS::CMSynergy::Use{cached_attributes};

    if ($VCS::CMSynergy::Use{tied_objects})
    {
	my $self = bless {}, $class;
	tie %$self, 'VCS::CMSynergy::ObjectTieHash', \%hash;
	return $self;
    }
    else
    {
	return bless \%hash, $class;
    }
}

# NOTE: All access to a VCS::CMSynergy::Objects data must either use
# methods, e.g. "$self->foo", or use _private(), e.g. "$self->_private->{foo}".
# Don't access its member directly, e.g. "$slef->{data}", becaus this
# doesn't work when :tied_objects are enabled.
# The only exception to this rule are the primary getter methods (objectname,
# version etc) which use direct access for speed. Hence they need to be
# redefined in ObjectTieHash.pm.

# access to private parts
sub _private 	{ shift; }
sub _acache 	{ shift->{acache}; }

sub list_attributes
{
    my ($self) = @_;
    return _list_attributes_cached($self->_private);
}

sub get_attribute
{
    my ($self, $attr_name) = @_;
    return _get_attribute_cached($self->_private, $attr_name);
}

sub set_attribute
{
    my ($self, $attr_name, $value) = @_;
    return _set_attribute_cached($self->_private, $attr_name, $value);
}

sub create_attribute
{
    my ($self, $attr_name, $type, $value) = @_;
    
    my $rc = $self->ccm->create_attribute($attr_name, $type, $value, $self);

    # update caches if necessary
    if ($rc)
    {
	my $attributes = $self->list_attributes;
	$attributes->{$attr_name} = $type;
	$self->_acache->{$attr_name} = $value
	    if $VCS::CMSynergy::Use{cached_attributes} && defined $value;
    }
    return $rc;
}

sub delete_attribute
{
    my ($self, $attr_name) = @_;

    my $rc = $self->ccm->delete_attribute($attr_name, $self);

    # update caches if necessary
    if ($rc)
    {
	my $attributes = $self->list_attributes;
	delete $attributes->{$attr_name};
	delete $self->_acache->{$attr_name}
	    if $VCS::CMSynergy::Use{cached_attributes};
    }
    return $rc;
}

# NOTE: No copy_attribute (yet) - what should happen 
# for $Use{cached_attributes}?

sub property
{
    my ($self, $keyword) = @_;
    $self->ccm->property($keyword, $self);
}

sub displayname
{
    my ($self) = @_;
    # cache this property (because it's immutable)
    $self->_private->{displayname} ||= $self->property('displayname');
}

sub cvid
{
    my ($self) = @_;
    # cache this property (because it's immutable)
    $self->_private->{cvid} ||= $self->property('cvid');
}


# non-method private subs

sub _list_attributes_cached
{
    my ($hash) = @_;
    return $hash->{attributes} ||= $hash->{ccm}->list_attributes($hash->{objectname});
}

sub _get_attribute_cached
{
    my ($hash, $attr_name) = @_;

    return $hash->{ccm}->get_attribute($attr_name, $hash->{objectname}) 
	unless $VCS::CMSynergy::Use{cached_attributes};

    return $hash->{acache}->{$attr_name} 
	if exists $hash->{acache}->{$attr_name};

    my $value = $hash->{ccm}->get_attribute($attr_name, $hash->{objectname});
    $hash->{acache}->{$attr_name} = $value if defined $value;
    return $value;
}

sub _set_attribute_cached
{
    my ($hash, $attr_name, $value) = @_;

    return $hash->{ccm}->set_attribute($attr_name, $hash->{objectname}, $value)
	unless $VCS::CMSynergy::Use{cached_attributes};

    $value = $hash->{ccm}->set_attribute($attr_name, $hash->{objectname}, $value);

    $hash->{acache}->{$attr_name} = $value if defined $value;
    return $value;
}

1;

__END__

=head1 DESCRIPTION

A C<VCS::CMSynergy::Object> is mostly a glorified wrapper for
a CM Synergy's objectname (sometimes called I<object reference form>
in CM Synergy documentation). Because of its overloaded string conversion
method (see below), it can be used with C<VCS::CMSynergy> methods wherever
an objectname would be appropriate, esp. where the documentation
specifies a B<file_spec>.

When L<VCS::CMSynergy::Object/:cached_attributes> is in effect,
a C<VCS::CMSynergy::Object> keeps a "demand loaded"
cache of attribute names and values.

There is also a L</TIEHASH INTERFACE> for 
manipulating an object's attributes using the hash notation.

=head1 BASIC METHODS

=head2 new

  # let $ccm be a VCS::CMSynergy
  $obj = VCS::CMSynergy::Object->new(
    $ccm, $name, $version, $cvtype, $instance);

  # more conveniently
  $obj = $ccm->object($name, $version, $cvtype, $instance);
  $obj2 = $ccm->object("name-version:cvtype:instance");

Create a C<VCS::CMSynergy::Object> from a CM Synergy session and
either an objectname
(sometimes called I<object reference form> in CM Synergy documentation)
in "name-version:cvtype:instance" format or the four parts specified
separately. 

Usually you would not call this method directly, but rather
via the wrapper L<VCS::CMSynergy/object>.

Note that no check is made whether the specified object really exists
in the iCM synergy database.

=head2 objectname 

  print $obj->objectname;

Returns the object's complete name in I<object reference form>,
i.e. C<"name-version:cvtype:instance"> where C<"-"> is meant as
a placeholder for the actual delimiter of the CM synergy database.

=head2 name, version, cvtype, instance

  print $obj->name;
  print $obj->version;
  print $obj->cvtype;
  print $obj->instance;

Returns the object's I<name>, I<version>, I<type>, or I<instance>, resp.
Note that I<instance> is also called I<subsystem> in older 
CM Synergy documentation.

=head2 string conversion

C<VCS::CMSynergy::Object> overloads string conversion with
L</objectname>, i.e. the following expressions evaluate to the same string:

  "$obj"  
  $obj->objectname

This makes it possible to use a C<VCS::CMSynergy::Object> throughout
C<VCS::CMSynergy> wherever an objectname would have been appropriate.

=head1 ATTRIBUTE METHODS

=head2 get_attribute, set_attribute

  print $obj->get_attribute($attribute_name);
  $obj->set_attribute($attribute_name) = "blurfl";

These are convenience wrappers for L<VCS::CMSynergy/get_attribute>
and L<VCS::CMSynergy/set_attribute>, resp., i.e.

  print $obj->get_attribute("comment");

is syntactic sugar for

  print $ccm->get_attribute(comment => $obj);

If you are C<use>ing L<VCS::CMSynergy/:cached_attributes>, 
these methods maintain a cache
of attribute names and values in the object. Note that this cache
is only consulted if you use C<VCS::CMSynergy::Object> methods
(including the L</TIEHASH INTERFACE>) and will get inconsistent if you
mix C<VCS::CMSynergy::Object> and C<VCS::CMSynergy> calls
on the same object.

=head2 create_attribute, delete_attribute

  $obj->create_attribute($attribute_name, $attribute_type);
  $obj->delete_attribute($attribute_name);

Convenience wrappers for L<VCS::CMSynergy/create_attribute> and  
L<VCS::CMSynergy/delete_attribute>, resp. Also update the cache
when L<VCS::CMSynergy/:cached_attributes> is in effect.

=head2 list_attributes

  $hashref = $obj->list_attributes;

Convenience wrapper for L<VCS::CMSynergy/list_attributes>. 

Note that the returned hash is always cached in the object
(and updated for successful L</create_attribute> and 
L</delete_attribute> calls).

=head1 PROPERTY METHODS

=head2 property

  $value = $obj->property($keyword);

Convenience wrapper for L<VCS::CMSynergy/property>, equivalent to

  $value = $ccm->property($keyword => $obj);

=head2 displayname, cvid

  print $obj->displayname;
  print $obj->cvid;

Short hand for C<< $obj->property("displayname") >> or
C<< $obj->property("cvid") >>, resp. However, these two methods
caches their return value in the C<VCS::CMSynergy::Object>
(because it is immutable).

=head1 TIEHASH INTERFACE

  use VCS::CMSynergy ':tied_objects';
  ...
  print $obj->{comment};	
  $obj->{comment} = "blurfl";	

When C<use>ing L<VCS::CMSynergy/:tied_objects>,
you can use a C<VCS::CMSynergy::Object> in the same way you
would use a hash reference. The available keys are the underlying
CM Synergy object's attributes. 

Note that contrary to the behaviour of real hashes, keys don't
spring into existence "on demand". Getting or setting the value
of an attribute that does not exist for the underlying CM Synergy object
will return C<undef> or throw an excpetion (depending on your sessions's
setting of L<VCS::CMSynergy/RaiseError>). 
However, testing for the existence of an attribute
with C<exists> works as expected.

NOTE: When using L<VCS::CMSynergy/:tied_objects>, it is strongly recommended
to have L<Scalar::Util|"the Scalar::Util module"> 
installed.  See L<Why is Scalar::Util recommended?> for an explanation.

=head2 FETCH, STORE

  $value = $obj->{attribute_name};
  $obj->{attribute_name} = $value;

These are wrappers for L</get_attribute> and L</set_attribute>, resp.
The operate on the same cache as these when
using  L<VCS::CMSynergy/:cached_attributes>

=head2 EXISTS

Checks the return value from L</list_attributes> for the existence
of the key (attribute) given.

=head2 FIRSTKEY, NEXTKEY

  foreach (@{ $obj->keys })  { ... }
  foreach (@{ $obj->values })  { ... }
  while (my ($attr, $val) = each %$obj)  { ... }

These methods use L</list_attributes> to obtain a list of attributes and then
iterate over this list. Hence C<keys>, C<values>, and C<each>
all work as expected. 

Warning: Enumerating the keys (i.e. attribute names) of a
tied  C<VCS::CMSynergy::Object>
is cheap (at most one call to B<ccm attribute -la>), but enumerating
the values may result in lots of calls to B<ccm attribute -show>.
Tools like L<Data::Dumper> or similar will implicitly enumerate all keys and
values when invoked on a tied object. This is especially annoying
when using the graphical Perl debugger L<Devel::ptkdb> and mousing
over a variable holding a tied object, because the debugger
uses  L<Data::Dumper> to construct a printable representation of
the object.

=head2 Why is Scalar::Util recommended?

Every C<VCS::CMSynergy::Object> keeps a reference to the session
(a C<VCS::CMSynergy>) where it was created in. It needs this "back pointer"
so it can implement methods that invoke CM Synergy operations,
e.g. L</get_attribute>. These references can
prevent the timely garbage collection of a session (esp. 
B<ccm stop> called from L<VCS::CMSynergy/DESTROY>) if a  
C<VCS::CMSynergy::Object> has a longer lifetime than its session.
(The latter is actually a programmer error, but there's no way
to enforce the correct lifetime rule.)
To work around this we need to make the session reference
not count w.r.t. to garbage collection. We use L<Scalar::Util/weaken> 
for that. If the C<VCS::CMSynergy> object goes away B<before> 
the C<VCS::CMSynergy::Object> gets destroyed, its session reference
will become C<undef>. 
Any method called on the C<VCS::CMSynergy::Object> after this point
that tries to invoke a session method 
will result in a (rightly deserved) error 
(C<Can't call method "..." on an undefined value>).

=head1 SEE ALSO

L<VCS::CMSynergy>
