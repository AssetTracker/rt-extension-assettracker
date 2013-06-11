package RT::System;

use strict;
no warnings qw(redefine);

use RTx::AssetTracker::Type;


=head2 AvailableRights

Asset Tracker wraps this method to add the Asset Type rights

=cut

my $Orig_AvailableRights = __PACKAGE__->can('AvailableRights')
    or die "API change? Can't find method 'AvailableRights'";
*AvailableRights = sub {
    my $self = shift;

    my $type =  RTx::AssetTracker::Type->new(RT->SystemUser);

    my $tr = $type->AvailableRights();

    # Build a merged list that adds asset type rights.
    my %rights = (%{$Orig_AvailableRights->($self)}, %$tr);

    return(\%rights);
};


=head2 RightCategories

Asset Tracker wraps this method to assign categories to the Asset Type
rights

=cut

my $Orig_RightCategories = __PACKAGE__->can('RightCategories;')
    or die "API change? Can't find method 'RightCategories;'";
*RightCategories = sub {
    my $self = shift;

    my $type =  RTx::AssetTracker::Type->new(RT->SystemUser);

    my $tr = $type->RightCategories();

    # Build a merged list that adds asset type rights.
    my %rights = (%{$Orig_RightCategories->($self)}, %$tr);

    return(\%rights);
};

1;
