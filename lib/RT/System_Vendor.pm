package RT::System;

use strict;
no warnings qw(redefine);

sub AvailableRights {
    my $self = shift;

    my $queue = RT::Queue->new(RT->SystemUser);
    my $group = RT::Group->new(RT->SystemUser);
    my $cf    = RT::CustomField->new(RT->SystemUser);
    my $class = RT::Class->new(RT->SystemUser);
    my $type =  RTx::AssetTracker::Type->new(RT->SystemUser);

    my $qr = $queue->AvailableRights();
    my $gr = $group->AvailableRights();
    my $cr = $cf->AvailableRights();
    my $clr = $class->AvailableRights();
    my $tr = $type->AvailableRights();

    # Build a merged list of all system wide rights, queue rights and group rights.
    my %rights = (%{$RT::System::RIGHTS}, %{$gr}, %{$qr}, %{$cr}, %{$clr}, %$tr);
    delete $rights{ExecuteCode} if RT->Config->Get('DisallowExecuteCode');

    return(\%rights);
}

sub RightCategories {
    my $self = shift;

    my $queue = RT::Queue->new(RT->SystemUser);
    my $group = RT::Group->new(RT->SystemUser);
    my $cf    = RT::CustomField->new(RT->SystemUser);
    my $class = RT::Class->new(RT->SystemUser);
    my $type =  RTx::AssetTracker::Type->new(RT->SystemUser);

    my $qr = $queue->RightCategories();
    my $gr = $group->RightCategories();
    my $cr = $cf->RightCategories();
    my $clr = $class->RightCategories();
    my $tr = $type->RightCategories();

    # Build a merged list of all system wide rights, queue rights and group rights.
    my %rights = (%{$RT::System::RIGHT_CATEGORIES}, %{$gr}, %{$qr}, %{$cr}, %{$clr}, %$tr);

    return(\%rights);
}

1;
