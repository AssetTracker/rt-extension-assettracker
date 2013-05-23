
package RT::CustomFields;

use strict;
no warnings qw(redefine);


=head2 LimitToType TYPEID

This is a copy of LimitToQueue which is
DEPRECATED since CFs are applicable not only to tickets these days.

Takes a type id (numerical) as its only argument. Makes sure that
Scopes it pulls out apply to this type (or another that you've selected with
another call to this method

=cut

sub LimitToType {
    my $self = shift;
    my $type = shift;

    $self->Limit (ALIAS => $self->_OCFAlias,
                  ENTRYAGGREGATOR => 'OR',
                  FIELD => 'ObjectId',
                  VALUE => "$type")
        if defined $type;
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}


=item LimitToGlobalOrType TYPEID

This is a copy of LimitToGlobalOrQueue which is
DEPRECATED since CFs are applicable not only to tickets these days.

Limits the set of custom fields found to global custom fields or those tied to the type with ID TYPEID

=cut

sub LimitToGlobalOrType {
    my $self = shift;
    my $type = shift;
    $self->LimitToGlobalOrObjectId( $type );
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}


=head2 LimitToGlobalAsset

This is a copy of LimitToGlobal which is
DEPRECATED since CFs are applicable not only to tickets these days.

Makes sure that Scopes it pulls out apply to all types
(or another that you've selected with
another call to this method or LimitToType)

=cut

sub LimitToGlobalAsset {
    my $self = shift;

    $self->Limit (ALIAS => $self->_OCFAlias,
                  ENTRYAGGREGATOR => 'OR',
                  FIELD => 'ObjectId',
                  VALUE => 0);
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}


1;
