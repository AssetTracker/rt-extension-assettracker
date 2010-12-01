
package RT::CustomField;

use strict;
no warnings qw(redefine);

use RT::CustomField;
use RT::CurrentUser;

# {{{ sub LoadByName

=head2  LoadByName (Type => QUEUEID, Name => NAME)

Loads the Custom field named NAME.

If a Type parameter is specified, only look for ticket custom fields tied to that Type.

If the Type parameter is '0', look for global ticket custom fields.

If no queue parameter is specified, look for any and all custom fields with this name.

BUG/TODO, this won't let you specify that you only want user or group CFs.

=cut

# Change after 3.4 beta.
*LoadByNameAndType = \&LoadByName;

sub LoadByName {
    my $self = shift;
    my %args = (
        Queue => undef,
        Type => undef,
        Name  => undef,
        @_,
    );

    # if we're looking for a queue by name, make it a number
    if  (defined $args{'Queue'}  &&  $args{'Queue'} !~ /^\d+$/) {
        my $QueueObj = RT::Queue->new($self->CurrentUser);
        $QueueObj->Load($args{'Queue'});
        $args{'Queue'} = $QueueObj->Id;
    }
    elsif  (defined $args{'Type'}  &&  $args{'Type'} !~ /^\d+$/) {
        my $TypeObj = RTx::AssetTracker::Type->new($self->CurrentUser);
        $TypeObj->Load($args{'Type'});
        $args{'Type'} = $TypeObj->Id;
    }

    # XXX - really naive implementation.  Slow. - not really. still just one query

    my $CFs = RT::CustomFields->new($self->CurrentUser);
    $CFs->SetContextObject( $self->ContextObject );

    $CFs->Limit( FIELD => 'Name', VALUE => $args{'Name'} );
    # Don't limit to queue if queue is 0.  Trying to do so breaks
    # RT::Group type CFs.
    if ( defined $args{'Queue'} ) {
        $CFs->LimitToQueue( $args{'Queue'} );
    }
    elsif (defined $args{'Type'}) {
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
