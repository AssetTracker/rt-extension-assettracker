=head1 NAME

  RTx::AssetTracker::Port - an AssetTracker Port object

=head1 SYNOPSIS

  use RTx::AssetTracker::Port;

=head1 DESCRIPTION


=head1 METHODS

=begin testing 

ok (require RTx::AssetTracker::Port);

=end testing

=cut


package RTx::AssetTracker::Port;

use strict;
no warnings qw(redefine);

use RTx::AssetTracker::Ports;

sub Create {

    my $self = shift;

    my %args = (
                Transport => '',
                Port => '',
                IP => '0',
                  @_);

    #return (1, "No right to modify asset") unless $self->AssetObj->CurrenUserHasRight('ModifyAsset');
    return (0, "Invalid transport. Must be TCP or UDP") unless $args{Transport} =~ /^(TCP|UDP)$/;
    return (0, "Port must be a number") unless ( $args{Port} =~ /^\d+$/ and $args{Port} > 0 );

    $self->SUPER::Create(
                         Transport => $args{'Transport'},
                         Port => $args{'Port'},
                         IP => $args{'IP'},
    );

}

sub LoadPort {

    my $self = shift;
    my %args = @_;

    my $Ports = RTx::AssetTracker::Ports->new( $self->CurrentUser );
    $Ports->Limit( FIELD => 'Transport', VALUE => $args{Transport} );
    $Ports->Limit( FIELD => 'Port', VALUE => $args{Port} );
    $Ports->Limit( FIELD => 'IP', VALUE => $args{IP} );

    #return (0, $Ports->Count . " ports found, 1 expected") if $Ports->Count > 1;
    return (0, "No ports found, 1 expected") if $Ports->Count == 0;

    $self->Load($Ports->First->Id);
    return ($Ports->First, "Port found") if $Ports->Count == 1;

}

1;
