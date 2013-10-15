
package RT::CustomFields;

use strict;
no warnings qw(redefine);


=head2 LimitToGlobalOrAssetType ASSETTYPEID

This is a copy of LimitToGlobalOrQueue which is
DEPRECATED since CFs are applicable not only to tickets these days.

Limits the set of custom fields found to global custom fields or those tied to the asset type with ID ASSETTYPEID

=cut

sub LimitToGlobalOrAssetType {
    my $self = shift;
    my $assettype = shift;
    $self->LimitToGlobalOrObjectId( $assettype );
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}


=head2 LimitToAssetType ASSETTYPEID

This is a copy of LimitToQueue which is
DEPRECATED since CFs are applicable not only to tickets these days.

Takes an asset type id (numerical) as its only argument. Makes sure that
Scopes it pulls out apply to this asset type (or another that you've selected with
another call to this method

=cut

sub LimitToAssetType {
    my $self = shift;
    my $assettype = shift;

    $self->Limit (ALIAS => $self->_OCFAlias,
                  ENTRYAGGREGATOR => 'OR',
                  FIELD => 'ObjectId',
                  VALUE => "$assettype")
        if defined $assettype;
    $self->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
}


=head2 LimitToGlobalAsset

This is a copy of LimitToGlobal which is
DEPRECATED since CFs are applicable not only to tickets these days.

Makes sure that Scopes it pulls out apply to all asset types
(or another that you've selected with
another call to this method or LimitToAssetType)

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
