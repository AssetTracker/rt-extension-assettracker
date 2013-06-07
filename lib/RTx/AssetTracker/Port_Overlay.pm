=head1 NAME

  RTx::AssetTracker::Port - an AssetTracker Port object

=head1 SYNOPSIS

  use RTx::AssetTracker::Port;

=head1 DESCRIPTION


=head1 METHODS

=cut


package RTx::AssetTracker::Port;

use strict;
no warnings qw(redefine);

use RTx::AssetTracker::Asset;
use RTx::AssetTracker::IP;

sub IPObj {

    my $self = shift;

    my $ip = RTx::AssetTracker::IP->new( $self->CurrentUser );
    $ip->Load($self->IP);
    return $ip;
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

# Port Transactions
    my $objs = RT::Transactions->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Type', VALUE => 'AddPort' );
    $objs->Limit( FIELD => 'ObjectType', VALUE => 'RTx::AssetTracker::Asset' );
    $objs->Limit( FIELD => 'ObjectId', VALUE => $self->IPObj->Asset );
    $objs->Limit( FIELD => 'NewValue', VALUE => $self->IPObj->IP . ' ' . $self->Transport . ' ' . $self->Port );
    push( @$list, $objs );

    $deps->_PushDependencies(
            BaseObject => $self,
            Flags => DEPENDS_ON,
            TargetObjects => $list,
            Shredder => $args{'Shredder'}
        );

    return $self->SUPER::__DependsOn( %args );
}


1;
