package RT::Transaction;

use strict;
no warnings qw(redefine);


=head2 Create

Asset Tracker wraps this method to enable the execution of Asset
Scrips

=cut

my $Orig_Create = __PACKAGE__->can('Create')
    or die "API change? Can't find method 'Create'";
*Create = sub {
    my $self = shift;
    my %args = (
        ActivateScrips => 1,
        CommitScrips => 1,
        @_
    );

    my ($id, $msg) = $Orig_Create->($self, @_);


    #Provide a way to turn off scrips if we need to
    $RT::Logger->debug('About to think about scrips for transaction #' .$self->Id);
    if ( $args{'ActivateScrips'} and $self->ObjectType eq 'RTx::AssetTracker::Asset' ) {
       $self->{'scrips'} = RTx::AssetTracker::Scrips->new($RT::SystemUser);

        $RT::Logger->debug('About to prepare scrips for transaction #' .$self->Id); 

        $self->{'scrips'}->Prepare(
            Stage       => 'TransactionCreate',
            Type        => $self->Type,
            Asset       => $self->ObjectId,
            Transaction => $self->id,
        );

        if ($args{'CommitScrips'} ) {
            $RT::Logger->debug('About to commit scrips for transaction #' .$self->Id);
            $self->{'scrips'}->Commit();
        }
    }

    return ( $id, $self->loc("Transaction Created") );
};


=head2 FriendlyObjectType

Asset Tracker wraps this method so history can just show eg "Asset" or
"Type" instead of the full class name

=cut

my $Orig_FriendlyObjectType = __PACKAGE__->can('FriendlyObjectType')
    or die "API change? Can't find method 'FriendlyObjectType'";
*FriendlyObjectType = sub {
    my $self = shift;
    my $type = $self->ObjectType or return undef;
    if ($type =~ s/^RTx::AssetTracker:://) {
        return $self->loc($type);
    }
    else
    {
        return $Orig_FriendlyObjectType->($self);
    }
};


=head2 BriefDescription

Asset Tracker adds new transaction types, adds more link types to the
AddLink and DeleteLink transaction types, and adds the asset Type
field to the Set transaction type.

=cut

$_BriefDescriptions{Update} = sub {
        my $self = shift;
        return $self->loc( "Asset update" );
    };

$_BriefDescriptions{AddIP} = sub {
        my $self = shift;
        return $self->loc( "IP address [_1] added", $self->NewValue);
    };

$_BriefDescriptions{DelIP} = sub {
        my $self = shift;
        return $self->loc( "IP address [_1] deleted", $self->OldValue);
    };

$_BriefDescriptions{AddPort} = sub {
        my $self = shift;
        return $self->loc( "Port [_1] added", $self->NewValue);
    };

$_BriefDescriptions{DelPort} = sub {
        my $self = shift;
        return $self->loc( "Port [_1] deleted", $self->OldValue);
    };

$_BriefDescriptions{TicketLink} = sub {
        my $self = shift;
        my $ticket = RT::Ticket->new( $self->CurrentUser );
        $ticket->Load($self->NewValue) or return $self->loc("Ticket #[_1] linked", $self->NewValue);
        return $self->loc( "Ticket #[_1] : [_2] ([_3])", $self->NewValue, $ticket->Subject, $ticket->Status);
    };

my $Orig_BriefDescriptions_AddLink = $_BriefDescriptions{AddLink};
$_BriefDescriptions{AddLink} = sub {
        my $self = shift;
        return $Orig_BriefDescriptions_AddLink->($self)
            unless ( $self->Data && $Orig_BriefDescriptions_AddLink->($self) eq $self->Data );
        my $value;
        if ( $self->NewValue ) {
            my $URI = RT::URI->new( $self->CurrentUser );
            $URI->FromURI( $self->NewValue );
            if ( $URI->Resolver ) {
                $value = $URI->Resolver->AsString;
            }
            else {
                $value = $self->NewValue;
            }
            if ( $self->Field eq 'DependsOn' ) {
                return $self->loc( "Dependency on [_1] added", $value );
            }
            elsif ( $self->Field eq 'DependedOnBy' ) {
                return $self->loc( "Dependency by [_1] added", $value );

            }
            elsif ( $self->Field eq 'RefersTo' ) {
                return $self->loc( "Reference to [_1] added", $value );
            }
            elsif ( $self->Field eq 'ReferredToBy' ) {
                return $self->loc( "Reference by [_1] added", $value );
            }
            elsif ( $self->Field eq 'ComponentOf' ) {
                return $self->loc( "Component of [_1] added", $value );
            }
            elsif ( $self->Field eq 'HasComponent' ) {
                return $self->loc( "Component [_1] added", $value );
            }
            elsif ( $self->Field eq 'IsRunning' ) {
                return $self->loc( "Is running [_1] added", $value );
            }
            elsif ( $self->Field eq 'RunsOn' ) {
                return $self->loc( "Runs on [_1] added", $value );
            }
            else {
                my $string = $self->Field;
                $string =~ s/([a-z])([A-Z])/$1 $2/g;
                return $self->loc( "$string [_1] added", $value );
            }
        }
        else {
            return ( $self->Data );
        }
    };

my $Orig_BriefDescriptions_DeleteLink = $_BriefDescriptions{DeleteLink};
$_BriefDescriptions{DeleteLink} = sub {
        my $self = shift;
        return $Orig_BriefDescriptions_DeleteLink->($self)
            unless ( $self->Data && $Orig_BriefDescriptions_DeleteLink->($self) eq $self->Data );
        my $value;
        if ( $self->OldValue ) {
            my $URI = RT::URI->new( $self->CurrentUser );
            $URI->FromURI( $self->OldValue );
            if ( $URI->Resolver ) {
                $value = $URI->Resolver->AsString;
            }
            else {
                $value = $self->OldValue;
            }

            if ( $self->Field eq 'DependsOn' ) {
                return $self->loc( "Dependency on [_1] deleted", $value );
            }
            elsif ( $self->Field eq 'DependedOnBy' ) {
                return $self->loc( "Dependency by [_1] deleted", $value );

            }
            elsif ( $self->Field eq 'RefersTo' ) {
                return $self->loc( "Reference to [_1] deleted", $value );
            }
            elsif ( $self->Field eq 'ReferredToBy' ) {
                return $self->loc( "Reference by [_1] deleted", $value );
            }
            elsif ( $self->Field eq 'ComponentOf' ) {
                return $self->loc( "Component of [_1] deleted", $value );
            }
            elsif ( $self->Field eq 'HasComponent' ) {
                return $self->loc( "Component [_1] deleted", $value );
            }
            elsif ( $self->Field eq 'IsRunning' ) {
                return $self->loc( "Is running [_1] deleted", $value );
            }
            elsif ( $self->Field eq 'RunsOn' ) {
                return $self->loc( "Runs on [_1] deleted", $value );
            }
            else {
                my $string = $self->Field;
                $string =~ s/([a-z])([A-Z])/$1 $2/g;
                return $self->loc( "$string [_1] deleted", $value );
            }
        }
        else {
            return ( $self->Data );
        }
    };

my $Orig_BriefDescriptions_Set = $_BriefDescriptions{Set};
$_BriefDescriptions{Set} = sub {
        my $self = shift;

        if ( $self->ObjectType eq 'RTx::AssetTracker::Asset' && $self->Field eq 'Type' ) {
            my $t1 = new RTx::AssetTracker::Type( $self->CurrentUser );
            $t1->Load( $self->OldValue );
            my $t2 = new RTx::AssetTracker::Type( $self->CurrentUser );
            $t2->Load( $self->NewValue );
            return $self->loc("[_1] changed from [_2] to [_3]",
                              $self->loc($self->Field) , $t1->Name , $t2->Name);
        }

        else {
            return $Orig_BriefDescriptions_Set->($self);
        }
};


=head2 CustomFieldLookupType

Returns the RT::Transaction lookup type, which can 
be passed to RT::CustomField->Create() via the 'LookupType' hash key.

=cut

my $Orig_CustomFieldLookupType = __PACKAGE__->can('CustomFieldLookupType')
    or die "API change? Can't find method 'CustomFieldLookupType'";
*CustomFieldLookupType = sub {
    my $self = shift;

    if ( ref $self && $self->{values}->{objecttype} eq 'RTx::AssetTracker::Asset' ) {
        return "RTx::AssetTracker::Type-RTx::AssetTracker::Asset-RT::Transaction";
    } else {
        return $Orig_CustomFieldLookupType->($self);
    }
};


1;
