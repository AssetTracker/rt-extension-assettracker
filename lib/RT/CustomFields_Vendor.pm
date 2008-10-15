
package RT::CustomFields;

use strict;

# {{{ sub LimitToType

=head2 LimitToType TYPEID

Takes a type id (numerical) as its only argument. Makes sure that
Scopes it pulls out apply to this type (or another that you've selected with
another call to this method

=cut

sub LimitToType  {
    my $self = shift;
    my $type = shift;

    $self->Limit (ALIAS => $self->_OCFAlias,
                  ENTRYAGGREGATOR => 'OR',
                  FIELD => 'ObjectId',
                  VALUE => "$type")
        if defined $type;
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}
# }}}

# {{{ sub LimitToGlobalOrType

=item LimitToGlobalOrType TYPEID

Limits the set of custom fields found to global custom fields or those tied to the type with ID TYPEID

=cut

sub LimitToGlobalOrType {
    my $self = shift;
    my $type = shift;
    $self->LimitToGlobalOrObjectId( $type );
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}

# }}}

# {{{ sub LimitToGlobalAsset

=head2 LimitToGlobalAsset

Makes sure that
Scopes it pulls out apply to all queues (or another that you've selected with
another call to this method or LimitToType

=cut


sub LimitToGlobalAsset  {
   my $self = shift;

  $self->Limit (ALIAS => $self->_OCFAlias,
                ENTRYAGGREGATOR => 'OR',
                FIELD => 'ObjectId',
                VALUE => 0);
  $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}
# }}}


1;
