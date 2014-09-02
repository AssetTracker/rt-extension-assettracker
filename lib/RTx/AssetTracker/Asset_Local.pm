no warnings qw(redefine);

sub Clone {

    # still need to handle Links
    my $self = shift;
    my $name = shift or return ( 0, 0, "No name specified for cloned asset." );

    # test name uniqueness
    my ($rv, $msg) = $self->SatisfiesUniqueness($name, $self->TypeObj->Id, $self->Status);
    return ($rv, 0, $msg) unless $rv;

    my @CustomFields;
    my $cfs = $self->CustomFields;
    while (my $cf = $cfs->Next) {
        # may have to skip unique CFs some day
        push @CustomFields, "CustomField-" . $cf->Id;

        my $cfvals = $self->CustomFieldValues($cf->Id);
        push @CustomFields, [ map { $_->Content } @{$cfvals->ItemsArrayRef} ];
    }

    my $asset = RTx::AssetTracker::Asset->new($self->CurrentUser);
    return $asset->Create(
         Type => $self->TypeObj,
         Name => $name,
         Description => $self->Description,
         Status => $self->Status,
         TransactionData => "Cloned from " . $self->Name,
         @CustomFields );
}

1;
