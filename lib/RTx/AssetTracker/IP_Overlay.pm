=head1 NAME

  RTx::AssetTracker::IP - an AssetTracker IP object

=head1 SYNOPSIS

  use RTx::AssetTracker::IP;

=head1 DESCRIPTION


=head1 METHODS

=begin testing 

ok (require RTx::AssetTracker::IP);

=end testing

=cut


package RTx::AssetTracker::IP;

use strict;
no warnings qw(redefine);

use RTx::AssetTracker::Asset;
use RTx::AssetTracker::Port;
use RTx::AssetTracker::IP;

# {{{ sub Create

=head2 Create

Create a new transaction.

This routine should _never_ be called anything other Than RTx::AssetTracker::Asset. It should not be called
from client code. Ever. Not ever.  If you do this, we will hunt you down. and break your kneecaps.
Then the unpleasant stuff will start.

TODO: Document what gets passed to this

=cut

sub Create {
    my $self = shift;
    my %args = (
        Asset          => undef,
        IP             => undef,
        Interface      => undef,
        MAC            => undef,
        TCPPorts       => [],
        UDPPorts       => [],
        Silent         => 0,
        SilentPorts    => 0,
        @_
    );

    my $exists = RTx::AssetTracker::IP->new( $RT::SystemUser );
    $exists->Load( $args{IP} );
    return (0, "IP address already in use by asset " . $exists->AssetObj->Name) if $exists->Id;

    my %ports = ( TCPPorts => $args{TCPPorts}, UDPPorts => $args{UDPPorts} );
    delete @args{ qw( TCPPorts UDPPorts ) };

    my ($rv, $msg) = $self->SUPER::Create(Asset => $args{Asset}, IP => $args{IP}, Interface => $args{Interface}, MAC => $args{MAC});
    return ($rv, "$msg ".$exists->AssetObj->Name) unless ($rv);

    my $ip = RTx::AssetTracker::IP->new( $self->CurrentUser );
    $ip->Load( $rv ) || return 0;

    #if success then add each port
    #do all this in a transaction????
    foreach my $tcp (@{$ports{TCPPorts}}) {
        $ip->AddPort( Transport => 'TCP',  Port => $tcp, Silent => $args{Silent} || $args{SilentPorts} )
    }
    foreach my $udp (@{$ports{UDPPorts}}) {
        $ip->AddPort( Transport => 'UDP',  Port => $udp, Silent => $args{Silent} || $args{SilentPorts} )
    }

    return ($rv, $msg);
}

# {{{ sub Load

=head2 Load

Takes either a numerical id or a textual Name and loads the specified IP.

=cut

sub Load {
    my $self = shift;

    my $identifier = shift;
    if ( !$identifier ) {
        return (undef);
    }

    if ( $identifier =~ /^(\d+)$/ ) {
        $self->SUPER::LoadById($identifier);
    }
    else {
        $self->LoadByCols( IP => $identifier );
    }

    return ( $self->Id );

}

# }}}

sub AssetObj {

    my $self = shift;

    my $asset = RTx::AssetTracker::Asset->new( $self->CurrentUser );
    $asset->Load($self->Asset);
    return $asset;
}

sub AddPort {
    my $self = shift;

    my %args = (
         Transport => '',
         Port => 0,
         TransactionData => undef,
         @_
    );

    unless ( $self->AssetObj->CurrentUserHasRight('ModifyAsset') || $self->AssetObj->CurrentUserHasRight('CreateAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    my $port = RTx::AssetTracker::Port->new( $self->CurrentUser );
    my ($rv, $msg) = $port->Create(IP => $self->Id, %args);

    if ($rv) {
        unless ( $args{'Silent'} ) {
            $self->AssetObj->_NewTransaction( Type     => 'AddPort',
                                    NewValue => $self->IP . " $args{Transport} $args{Port}",
                                    Field    => 'Port', Data => $args{TransactionData} );
        }

        return ( 1, $self->loc( "$args{Transport} $args{Port} has been added for this IP." ) );
    } else {
        return(0, $self->loc("Port could not be added: $msg"));
    }

}

sub AddTCPPort {
    my $self = shift;
    my $port = shift;

    return $self->AddPort( Transport => 'TCP',  Port => $port, @_ );
}

sub AddUDPPort {
    my $self = shift;
    my $port = shift;

    return $self->AddPort( Transport => 'UDP',  Port => $port, @_ );
}

sub DeletePort {

    my $self = shift;
    my %args = (
        Transport        => undef,
        Port             => undef,
        TransactionData             => undef,
        @_ );

    unless ( $self->AssetObj->CurrentUserHasRight('ModifyAsset') || $self->AssetObj->CurrentUserHasRight('CreateAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    my $port = RTx::AssetTracker::Port->new( $self->CurrentUser );
    $port->LoadPort( IP => $self->Id, %args );
    my $portnum = $port->Port;
    my $transport = $port->Transport;

    # If we can't find this IP, we need to bail.
    unless ( $port->Id ) {
        return ( 0, $self->loc("Could not find that $args{Transport} Port") );
    }

    my $retval = $port->Delete();
    if ($retval) {
        unless ( $args{'Silent'} ) {
            $self->AssetObj->_NewTransaction( Type     => 'DelPort',
                                    OldValue => $self->IP . " $transport $portnum",
                                    Field    => 'Port', Data => $args{TransactionData} );
        }

        return ( 1, $self->loc( "$portnum $transport is no longer an port for this IP." ) );
    } else {
        return(0, $self->loc("Port could not be deleted"));
    }

}

sub DeleteTCPPort {
    my $self = shift;
    my $port = shift;

    return $self->DeletePort( Transport => 'TCP',  Port => $port, @_ );
}

sub DeleteUDPPort {
    my $self = shift;
    my $port = shift;

    return $self->DeletePort( Transport => 'UDP',  Port => $port, @_ );
}

sub TCPPorts {
    my $self = shift;

    my $Ports = RTx::AssetTracker::Ports->new( $self->CurrentUser );
    $Ports->LimitToIP($self->Id);
    $Ports->LimitToTCP;

    my @ports;
    while (my $port = $Ports->Next) {

        push @ports, $port->Port;

    }

    return @ports;

}

sub UDPPorts {
    my $self = shift;

    my $Ports = RTx::AssetTracker::Ports->new( $self->CurrentUser );
    $Ports->LimitToIP($self->Id);
    $Ports->LimitToUDP;

    my @ports;
    while (my $port = $Ports->Next) {

        push @ports, $port->Port;

    }

    return @ports;

}

sub DeleteAllPorts {
    my $self = shift;

    my $Ports = RTx::AssetTracker::Ports->new( $self->CurrentUser );
    $Ports->LimitToIP($self->Id);
    my $count = $Ports->Count;
    my $success = 0;

    while (my $port = $Ports->Next) {
        my $retval = $port->Delete(RecordTransaction => 0);
        $success++ if $retval;
    }

    if ($success == $count) {
        return( 1, "All ports deleted");
    }

    return( 0, "$success of $count  ports deleted");
   

}


1;
