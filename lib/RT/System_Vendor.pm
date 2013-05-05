package RT::System;

use strict;
no warnings qw(redefine);

use RTx::AssetTracker::Type;

my $Orig_AvailableRights = \&AvailableRights;
*AvailableRights = sub {
    my $self = shift;

    my $type =  RTx::AssetTracker::Type->new(RT->SystemUser);

    my $tr = $type->AvailableRights();

    # Build a merged list of all system wide rights, queue rights and group rights.
    my %rights = (%{$Orig_AvailableRights->($self)}, %$tr);

    return(\%rights);
};


my $Orig_RightCategories = \&RightCategories;
*RightCategories = sub {
    my $self = shift;

    my $type =  RTx::AssetTracker::Type->new(RT->SystemUser);

    my $tr = $type->RightCategories();

    # Build a merged list of all system wide rights, queue rights and group rights.
    my %rights = (%{$Orig_RightCategories->($self)}, %$tr);

    return(\%rights);
};

1;
