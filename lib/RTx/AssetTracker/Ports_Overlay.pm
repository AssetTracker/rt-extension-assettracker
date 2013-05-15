=head1 NAME

  RTx::AssetTracker::Ports - a collection of AssetTracker Ports objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Ports;

=head1 DESCRIPTION


=head1 METHODS

=begin testing 

ok (require RTx::AssetTracker::Ports);

=end testing

=cut


package RTx::AssetTracker::Ports;

use strict;
no warnings qw(redefine);

sub LimitToIP {
    my $self = shift;

    my $IPid = shift;

    my %args = (
          Operator => '=',
          @_
    );

    $self->Limit( FIELD => 'IP', VALUE => $IPid, %args );
}

sub LimitToTransport {
    my $self = shift;

    my $transport = shift;
    my %args = (
          Operator => '=',
          @_
    );

    $self->Limit( FIELD => 'Transport', VALUE => $transport, %args );
}

sub LimitToTCP {
    my $self = shift;

    my %args = (
          Operator => '=',
          @_
    );

    $self->LimitToTransport('TCP', %args);
}

sub LimitToUDP {
    my $self = shift;

    my %args = (
          Operator => '=',
          @_
    );

    $self->LimitToTransport('UDP', %args);
}

1;
