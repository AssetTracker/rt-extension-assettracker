
package RT::CustomField;

use strict;
no warnings qw(redefine);

use RT::CustomField;
use RT::CurrentUser;


=head2 LoadByNameAndAssetType (Type => TYPEID, Name => NAME)

Loads the Custom field named NAME.

Will load a Disabled Custom Field even if there is a non-disabled Custom Field
with the same Name.

If a Type parameter is specified, only look for asset custom fields tied to that Type.

If the Type parameter is '0', look for global asset custom fields.

If no type parameter is specified, look for any and all custom fields with this name.

=cut

sub LoadByNameAndAssetType {
    my $self = shift;
    my %args = (
        Type => undef,
        Name  => undef,
        @_,
    );

    unless ( defined $args{'Name'} && length $args{'Name'} ) {
        $RT::Logger->error("Couldn't load Custom Field without Name");
        return wantarray ? (0, $self->loc("No name provided")) : 0;
    }

    # if we're looking for a type by name, make it a number
    if ( defined $args{'Type'} && ($args{'Type'} =~ /\D/ || !$self->ContextObject) ) {
        my $TypeObj = RTx::AssetTracker::Type->new( $self->CurrentUser );
        $TypeObj->Load( $args{'Type'} );
        $args{'Type'} = $TypeObj->Id;
        $self->SetContextObject( $TypeObj )
            unless $self->ContextObject;
    }

    # XXX - really naive implementation.  Slow. - not really. still just one query

    my $CFs = RT::CustomFields->new( $self->CurrentUser );
    $CFs->SetContextObject( $self->ContextObject );
    my $field = $args{'Name'} =~ /\D/? 'Name' : 'id';
    $CFs->Limit( FIELD => $field, VALUE => $args{'Name'}, CASESENSITIVE => 0);
    # Don't limit to type if type is 0.  Trying to do so breaks
    # RT::Group type CFs.
    if (defined $args{'Type'}) {
        $CFs->LimitToType( $args{'Type'} );
    }

    # When loading by name, we _can_ load disabled fields, but prefer
    # non-disabled fields.
    $CFs->FindAllRows;
    $CFs->OrderByCols(
        { FIELD => "Disabled", ORDER => 'ASC' },
    );

    # We only want one entry.
    $CFs->RowsPerPage(1);

    # version before 3.8 just returns 0, so we need to test if wantarray to be
    # backward compatible.
    return wantarray ? (0, $self->loc("Not found")) : 0 unless my $first = $CFs->First;

    return $self->LoadById( $first->id );
}


1;
