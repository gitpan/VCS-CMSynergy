package VCS::CMSynergy::ObjectTieHash;

# TIEHASH(class, { ccm => ..., name => ..., ...})
sub TIEHASH
{
    my ($class, $href) = @_;
    return bless $href, $class;
}

sub FETCH
{
    my ($self, $key) = @_;
    return VCS::CMSynergy::Object::_get_attribute_cached($self, $key);
}

sub STORE
{
    my ($self, $key, $value) = @_;
    return VCS::CMSynergy::Object::_set_attribute_cached($self, $key, $value);
}

sub EXISTS
{
    my ($self, $key) = @_;
    my $attributes = VCS::CMSynergy::Object::_list_attributes_cached($self);
    return exists $attributes->{$key};
}

sub FIRSTKEY
{
    my ($self) = @_;
    my $attributes = VCS::CMSynergy::Object::_list_attributes_cached($self);
    my $dummy = keys %$attributes;		# reset each() iterator
    return each %$attributes;
}

sub NEXTKEY
{
    my ($self, $lastkey) = @_;
    my $attributes = VCS::CMSynergy::Object::_list_attributes_cached($self);
    return each %$attributes;
}


# redefine getter methods and access to private data
{
    package VCS::CMSynergy::Object;

    no strict 'refs';
    no warnings 'redefine';

    foreach my $method (qw(objectname ccm name version cvtype instance))
    {
	*{$method} = sub { my $self = shift; (tied %$self)->{$method}; };
    }

    # access private parts via the tied object
    sub _private	{ my $self = shift; tied %$self; }
    sub _acache		{ my $self = shift; (tied %$self)->{acache}; }
}


1;
