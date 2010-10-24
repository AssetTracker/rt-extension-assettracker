
package RT::Ticket;

use strict;
no warnings qw(redefine);

my $Orig_AddLink = \&_AddLink;

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

my $Orig_LINKTYPEMAP = \&LINKTYPEMAP;

*LINKTYPEMAP = sub {

    my $self = shift;
    my $ticket_map = $Orig_LINKTYPEMAP->($self);
    my $asset_map = RTx::AssetTracker::Asset->LINKTYPEMAP();

    return { %$asset_map, %$ticket_map };
};

1;
