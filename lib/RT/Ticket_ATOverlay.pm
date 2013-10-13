
package RT::Ticket;

use strict;
no warnings qw(redefine);


=head2 _AddLink

Asset Tracker wraps this method to add a "TicketLink" transaction when
an asset is referred to by a ticket

=cut

my $Orig_AddLink = __PACKAGE__->can('_AddLink')
    or die "API change? Can't find method '_AddLink'";
*_AddLink = sub {

    my $self = shift;
    my ($linkid, $msg) = $Orig_AddLink->($self, @_);

    return ($linkid, $msg) unless $linkid;

    my $linkObj = RT::Link->new( $self->CurrentUser );
    my ($LinkId, $Msg) = $linkObj->Load($linkid);

    $LinkId or return ($linkid, $msg);

    my $TargetObj = $linkObj->TargetObj();
    my $BaseObj = $linkObj->BaseObj();

    return ($linkid, $msg) unless (ref $BaseObj eq 'RT::Ticket');

    if (ref $TargetObj eq 'RTx::AssetTracker::Asset') {

        $TargetObj->_NewTransaction(
            Type     => 'TicketLink',
            NewValue => $BaseObj->Id,
        );

    }

    return ($linkid, $msg);

};


1;
