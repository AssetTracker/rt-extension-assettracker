# BEGIN BPS TAGGED BLOCK {{{
#
# COPYRIGHT:
#
# This software is Copyright (c) 1996-2013 Best Practical Solutions, LLC
#                                          <sales@bestpractical.com>
#
# (Except where explicitly superseded by other copyright notices)
#
#
# LICENSE:
#
# This work is made available to you under the terms of Version 2 of
# the GNU General Public License. A copy of that license should have
# been provided with this software, but in any event can be snarfed
# from www.gnu.org.
#
# This work is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 or visit their web page on the internet at
# http://www.gnu.org/licenses/old-licenses/gpl-2.0.html.
#
#
# CONTRIBUTION SUBMISSION POLICY:
#
# (The following paragraph is not intended to limit the rights granted
# to you to modify and distribute this software under the terms of
# the GNU General Public License and is only of importance to you if
# you choose to contribute your changes and enhancements to the
# community by submitting them to Best Practical Solutions, LLC.)
#
# By intentionally submitting any modifications, corrections or
# derivatives to this work, or any other work intended for use with
# Request Tracker, to Best Practical Solutions, LLC, you confirm that
# you are the copyright holder for those contributions and you grant
# Best Practical Solutions,  LLC a nonexclusive, worldwide, irrevocable,
# royalty-free, perpetual, license to use, copy, create derivative
# works based on those contributions, and sublicense and distribute
# those contributions and any derivatives thereof.
#
# END BPS TAGGED BLOCK }}}

=head1 NAME

  RTx::AssetTracker::IP - an AssetTracker IP object

=head1 SYNOPSIS

  use RTx::AssetTracker::IP;

=head1 DESCRIPTION


=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::IP;
use base 'RTx::AssetTracker::Record';

sub Table {'AT_IPs'};

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

sub Delete {
    my $self = shift;

    my ($rv, $msg) = $self->DeleteAllPorts;
    if ($rv) {
        return $self->SUPER::Delete();
    }
    else {
        return $rv, $msg;
    }
}

## Shredder methods ##
use RT::Shredder::Constants;
use RT::Shredder::Exceptions;
use RT::Shredder::Dependencies;

sub __DependsOn
{
    my $self = shift;
    my %args = (
            Shredder => undef,
            Dependencies => undef,
            @_,
           );
    my $deps = $args{'Dependencies'};
    my $list = [];

# IP Ports
    my $objs = RTx::AssetTracker::Ports->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'IP', VALUE => $self->Id );
    push( @$list, $objs );

#IP Transactions
    $objs = RT::Transactions->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Type', VALUE => 'AddIP' );
    $objs->Limit( FIELD => 'ObjectType', VALUE => 'RTx::AssetTracker::Asset' );
    $objs->Limit( FIELD => 'ObjectId', VALUE => $self->Asset );
    $objs->Limit( FIELD => 'NewValue', VALUE => $self->IP );
    push( @$list, $objs );

#TODO: Users, Types if we wish export tool
    $deps->_PushDependencies(
            BaseObject => $self,
            Flags => DEPENDS_ON,
            TargetObjects => $list,
            Shredder => $args{'Shredder'}
        );

    return $self->SUPER::__DependsOn( %args );
}



=head2 id

Returns the current value of id. 
(In the database, id is stored as int(11).)


=cut


=head2 IP

Returns the current value of IP. 
(In the database, IP is stored as varchar(15).)



=head2 SetIP VALUE


Set IP to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, IP will be stored as a varchar(15).)


=cut


=head2 MAC

Returns the current value of MAC. 
(In the database, MAC is stored as varchar(12).)



=head2 SetMAC VALUE


Set MAC to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, MAC will be stored as a varchar(12).)


=cut


=head2 Interface

Returns the current value of Interface. 
(In the database, Interface is stored as varchar(25).)



=head2 SetInterface VALUE


Set Interface to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Interface will be stored as a varchar(25).)


=cut


=head2 Asset

Returns the current value of Asset. 
(In the database, Asset is stored as int(11).)



=head2 SetAsset VALUE


Set Asset to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Asset will be stored as a int(11).)


=cut


=head2 Creator

Returns the current value of Creator. 
(In the database, Creator is stored as int(11).)


=cut


=head2 Created

Returns the current value of Created. 
(In the database, Created is stored as datetime.)


=cut


=head2 LastUpdatedBy

Returns the current value of LastUpdatedBy. 
(In the database, LastUpdatedBy is stored as int(11).)


=cut


=head2 LastUpdated

Returns the current value of LastUpdated. 
(In the database, LastUpdated is stored as datetime.)


=cut



sub _CoreAccessible {
    {
     
        id =>
		{read => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => ''},
        IP => 
		{read => 1, write => 1, sql_type => 12, length => 15,  is_blob => 0,  is_numeric => 0,  type => 'varchar(15)', default => ''},
        MAC => 
		{read => 1, write => 1, sql_type => 12, length => 12,  is_blob => 0,  is_numeric => 0,  type => 'varchar(12)', default => ''},
        Interface => 
		{read => 1, write => 1, sql_type => 12, length => 25,  is_blob => 0,  is_numeric => 0,  type => 'varchar(25)', default => '0'},
        Asset => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Creator => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Created => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        LastUpdatedBy => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        LastUpdated => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},

 }
};

RT::Base->_ImportOverlays();

1;
