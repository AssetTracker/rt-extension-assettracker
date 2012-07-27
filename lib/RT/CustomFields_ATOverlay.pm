
package RT::CustomFields;

use strict;
no warnings qw(redefine);


=head2 LimitToAssetType ASSETTYPEID

Takes a numeric C<ASSETTYPEID>, and limits the Custom Field collection to
those only applied directly to it; this limit is OR'd with other
L</LimitToAssetType> and L</LimitToGlobalAsset> limits.

Note that this will cause the collection to only return asset CFs.

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

Limits the Custom Field collection to global asset CFs; this limit is
OR'd with L</LimitToAssetType> limits.

Note that this will cause the collection to only return asset CFs.

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
