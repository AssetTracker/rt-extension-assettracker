=head1 NAME

  RTx::AssetTracker::Asset - an AssetTracker Asset object

=head1 SYNOPSIS

  use RTx::AssetTracker::Asset;

=head1 DESCRIPTION


=head1 METHODS

=begin testing 

use RTx::AssetTracker::Asset;

=end testing

=cut


package RTx::AssetTracker::Asset;

use strict;
no warnings qw(redefine);

use RTx::AssetTracker::Type;
use RTx::AssetTracker::Assets;
use RT::Group;
use RT::URI::at;
use RTx::AssetTracker::IPs;
use RTx::AssetTracker::Ports;
use RT::URI;
use RT::CustomField;

if ($RT::VERSION gt '3.4.1' or $RT::VERSION eq '0.0.0' ) {
    RT::CustomField->_ForObjectType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' => "Assets" );
} else {
    $RT::CustomField::FRIENDLY_OBJECT_TYPES{'RTx::AssetTracker::Type-RTx::AssetTracker::Asset'} = "Assets";
}

# {{{ LINKTYPEMAP
# A helper table for links mapping to make it easier
# to build and parse links between assets

use vars '%LINKMAP';
use vars '%LINKTYPEMAP';
use vars '%LINKDIRMAP';
use vars '@LINKORDER';

%LINKMAP = (
    RunsOn      => 'IsRunning',
    RefersTo    => 'ReferredToBy',
    DependsOn   => 'DependedOnBy',
    ComponentOf => 'HasComponent',
);

@LINKORDER   = ();
%LINKTYPEMAP = ();
%LINKDIRMAP  = ();

while ( my ($base, $target) = each %LINKMAP ) {
    RTx::AssetTracker::Asset->RegisterLinkType( $base, $target );
}


sub RegisterLinkType {
    my $class = shift;

    my $base   = shift;
    my $target = shift;

    #return if exists $LINKTYPEMAP{$base};

    $LINKTYPEMAP{$base}{Type} = $base;
    $LINKTYPEMAP{$base}{Mode} = 'Target';
    my $base_name = $base;
    $base_name =~ s/([a-z])([A-Z])/$1 $2/g;
    $LINKTYPEMAP{$base}{Name} = $base_name;
    $LINKTYPEMAP{$base}{Mate} = $target;

    $LINKTYPEMAP{$target}{Type} = $base;
    $LINKTYPEMAP{$target}{Mode}   = 'Base';
    my $target_name = $target;
    $target_name =~ s/([a-z])([A-Z])/$1 $2/g;
    $LINKTYPEMAP{$target}{Name} = $target_name;
    $LINKTYPEMAP{$target}{Mate} = $base;

    $LINKDIRMAP{$base} = { Base => $base, Target => $target };

    push @LINKORDER, $base, $target;

    {
 
    no strict 'refs';

    *$base   = sub {
        my $self = shift;
        return ( $self->_Links( $LINKTYPEMAP{$target}{Mode}, $base ) );
    };

    *$target = sub {
        my $self = shift;
        return ( $self->_Links( $LINKTYPEMAP{$base}{Mode},   $base ) );
    };

    }

    # sets up the Limit methods for links
    #RTx::AssetTracker::Assets::RegisterLinkField($base, $target);
    $RTx::AssetTracker::Assets::FIELDS{$base}   = [ 'LINK' => To   => $base ];
    $RTx::AssetTracker::Assets::FIELDS{$target} = [ 'LINK' => From => $base ];

    {
        no strict 'refs';
        package RTx::AssetTracker::Assets;

        my $limit_base   = "Limit$base";
        my $limit_target = "Limit$target";

        *$limit_base = sub {
            my $self = shift;
            my $asset_uri = shift;
            $self->LimitLinkedTo ( TARGET => $asset_uri,
                                   TYPE => $base,          );

        };
        *$limit_target = sub {
            my $self = shift;
            my $asset_uri = shift;
            $self->LimitLinkedFrom ( BASE => $asset_uri,
                                     TYPE => $target,        );

        };

    }


}

# }}}


sub LINKMAP       { return \%LINKMAP   }
sub LINKTYPEMAP   { return \%LINKTYPEMAP   }
sub LINKDIRMAP    { return \%LINKDIRMAP   }
sub LINKORDER     { return  @LINKORDER   }

sub ConfigureLinks {
    my $class = shift;

    if (@RT::AssetLinkTypes and ! (@RT::AssetLinkTypes % 2)) {
        my @local_links = @RT::AssetLinkTypes;
        while ( @local_links ) {
            my $forward = shift @local_links;
            my $reverse = shift @local_links;
            $class->RegisterLinkType( $forward, $reverse );
        }
    }

}



# {{{ sub Load

=head2 Load

Takes a single argument. This can be a asset id or
asset name.  If the asset can't be loaded, returns undef.
Otherwise, returns the asset id.

=cut

sub Load {
    my $self = shift;
    my $id   = shift;

    #If we have an integer URI, load the asset
    if ( $id =~ /^\d+$/ ) {
        my ($assetid,$msg) = $self->LoadById($id);

        unless ($self->Id) {
            $RT::Logger->crit("$self tried to load a bogus asset: $id\n");
            return (undef);
        }
    }

    elsif ( $id ) {
        my ($assetid,$msg) = $self->LoadByCol('Name', $id);

        unless ($self->Id) {
            $RT::Logger->crit("$self tried to load a bogus asset named: $id\n");
            return (undef);
        }
    }

    else {
        $RT::Logger->warning("Tried to load a bogus asset id: '$id'");
        return (undef);
    }

    #Ok. we're loaded. lets get outa here.
    return ( $self->Id );

}

# }}}

# {{{ sub Create

=head2 Create (ARGS)

Arguments: ARGS is a hash of named parameters.  Valid parameters are:

  id
  Type  - Either a Type object or a Type Name
  Name -- The unique name of the asset
  Description -- A string describing the asset
  Status -- any valid status (Defined in RTx::AssetTracker::Type)
  Owner -- A reference to a list of  email addresses or Names
  Admin -- A reference to a list of  email addresses or Names
  CustomField-<n> -- a scalar or array of values for the customfield with the id <n>
  IP Address -- A reference to a list of either bare IP addresses or hashes of the appropiate form.


Returns: ASSETID, Transaction Object, Error Message


=begin testing

my $a = RTx::AssetTracker::Asset->new($RT::SystemUser);

my ($id, undef, undef) = $a->Create(Type => 'Servers', Name => "An test asset $$", Description => 'This is a description');
ok( $a->Id, "Asset Created");

ok ( my $id = $a->Id, "Got asset id");

=end testing

=cut

sub Create {
    my $self = shift;

    my %args = (
        id                 => undef,
        Type               => undef,
        Name               => undef,
        Description        => '',
        Status             => 'production',
        Owner              => undef,
        Admin              => undef,
        TransactionData    => undef,
        _RecordTransaction => 1,
        _Commit            => 1,
        @_
    );

    unless ($args{'_Commit'}) {
        $args{'_RecordTransaction'} = 0;
    }

    my ($ErrStr, @non_fatal_errors);

    my $TypeObj = RTx::AssetTracker::Type->new($RT::SystemUser);

    if ( ( defined( $args{'Type'} ) ) && ( !ref( $args{'Type'} ) ) ) {
        $TypeObj->Load( $args{'Type'} );
    }
    elsif ( ref( $args{'Type'} ) eq 'RTx::AssetTracker::Type' ) {
        $TypeObj->Load( $args{'Type'}->Id );
    }
    else {
        $RT::Logger->debug( $args{'Type'} . " not a recognized type object." );
    }

    #Can't create a asset without a type.
    unless ( defined($TypeObj) && $TypeObj->Id ) {
        $RT::Logger->debug("$self No type given for asset creation.");
        return ( 0, 0, $self->loc('Could not create asset. Type not set') );
    }

    #Now that we have a type, Check the ACLS
    unless (
        $self->CurrentUser->HasRight(
            Right  => 'CreateAsset',
            Object => $TypeObj,
            EquivObjects => [ $RTx::AssetTracker::System ],
        )
      )
    {
        return (
            0, 0,
            $self->loc( "No permission to create assets of this type '[_1]'", $TypeObj->Name));
    }

    unless ( $TypeObj->IsValidStatus( $args{'Status'} ) ) {
        return ( 0, 0, $self->loc('Invalid value for status') );
    }


    # test name uniqueness
    my ($rv, $msg) = $self->SatisfiesUniqueness($args{Name}, $TypeObj->Id, $args{Status});
    return ($rv, 0, $msg) unless $rv;


    $RT::Handle->BeginTransaction() if $args{'_Commit'};

    my %params = (
        Type            => $TypeObj->Id,
        Name            => $args{'Name'},
        Description     => $args{'Description'},
        Status          => $args{'Status'},
    );

# Parameters passed in during an import that we probably don't want to touch, otherwise
    foreach my $attr qw(id Creator Created LastUpdated LastUpdatedBy) {
        $params{$attr} = $args{$attr} if ( $args{$attr} );
    }

    my ($id,$asset_message) = $self->SUPER::Create( %params);
    unless ($id) {
        $RT::Logger->crit( "Couldn't create a asset: " . $asset_message );
        $RT::Handle->Rollback();
        return ( 0, 0,
            $self->loc("Asset could not be created due to an internal error")
        );
    }

    my $create_groups_ret = $self->_CreateAssetGroups( _RecordTransaction => $args{_RecordTransaction}, _Commit => $args{_Commit} );
    unless ($create_groups_ret) {
        $RT::Logger->crit( "Couldn't create asset groups for asset "
              . $self->Id
              . ". aborting Asset creation." );
        $RT::Handle->Rollback();
        return ( 0, 0,
            $self->loc("Asset could not be created due to an internal error")
        );
    }

    #Deal with setting up watchers
    foreach my $type ( RTx::AssetTracker::Type->ActiveRoleArray() ) {
        next unless ( defined $args{$type} );
        foreach my $watcher (
            ref( $args{$type} ) ? @{ $args{$type} } : ( $args{$type} ) )
        {

            # If there is an empty entry in the list, let's get out of here.
            next unless $watcher;

            # we reason that all-digits number must be a principal id, not email
            # this is the only way to can add
            my $field = 'Email';
            if ($watcher =~ /^\d+$/) {
                $field = 'PrincipalId';
            }
            elsif (my ($group_name) = $watcher =~ /^@(.+)$/) {
                my $group = RT::Group->new($self->CurrentUser);
                $group->LoadUserDefinedGroup($group_name);
                if ($group->Id) {
                    $field = 'PrincipalId';
                    $watcher = $group->PrincipalId;
                }
            }

            my ( $wval, $wmsg );

            # Note that we're using AddWatcher, rather than _AddWatcher, as we
            # actually _want_ that ACL check. Otherwise, random asset creators
            # could make themselves adminccs and maybe get asset rights. that would
            # be poor
            ( $wval, $wmsg ) = $self->AddWatcher(
                Type   => $type,
                $field => $watcher,
                Silent => 1
            );

            push @non_fatal_errors, $wmsg unless ($wval);
        }
    }


    #Add all the custom fields
    foreach my $arg ( keys %args ) {
        next unless ( $arg =~ /^CustomField-(\d+)$/i );
        my $cfid = $1;
        foreach
          my $value ( UNIVERSAL::isa( $args{$arg} => 'ARRAY' ) ? @{ $args{$arg} } : ( $args{$arg} ) )
        {
            next unless ( length($value) );

            # Allow passing in uploaded LargeContent etc by hash reference
            $self->_AddCustomFieldValue(
                (UNIVERSAL::isa( $value => 'HASH' )
                    ? %$value
                    : (Value => $value)
                ),
                Field             => $cfid,
                RecordTransaction => 0,
            );
        }
    }


    #Add links
    my ($addlink_rv, $addlink_errors) = $self->_AddLinksOnCreateOrUpdate(%args);
    push @non_fatal_errors, @$addlink_errors;

    #Add IP addresses
    my $ips = defined $args{'IP Address'} ? $args{'IP Address'} : [];
    for my $ip (@$ips) {
        my $ip_ref = {};
        if (!ref $ip) {
            $ip = { IP => $ip };
        }

        my ($rv, $msg) = $self->AddIP(%$ip, Silent => 1, SilentPorts => 1);
        return (0,0,$msg) unless ($rv);
    }


    # We override the URI lookup. the whole reason
    # we have a URI column is so that joins on the links table
    # aren't expensive and stupid
    my $uri = RT::URI::at->new( $self->CurrentUser );
    $self->__Set( Field => 'URI', Value => $uri->URIForObject($self) );


    if ( $args{'_RecordTransaction'} ) {

        # {{{ Add a transaction for the create
        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
                                                     Type      => "Create",
                                                     Data      => $args{TransactionData},
        );

        if ( $self->Id && $Trans ) {

            $TransObj->UpdateCustomFields(ARGSRef => \%args);

            $RT::Logger->info( "Asset " . $self->Id . " created of type '" . $TypeObj->Name . "' by " . $self->CurrentUser->Name );
            $ErrStr = $self->loc( "Asset [_1] created of type '[_2]'", $self->Id, $TypeObj->Name );
            $ErrStr = join( "\n", $ErrStr, @non_fatal_errors );
        }
        else {
            $RT::Handle->Rollback();

            $ErrStr = join( "\n", $ErrStr, @non_fatal_errors );
            $RT::Logger->error("Asset couldn't be created: $ErrStr");
            return ( 0, 0, $self->loc( "Asset could not be created due to an internal error"));
        }

        $RT::Handle->Commit();
        return ( $self->Id, $TransObj->Id, $ErrStr );

        # }}}
    }
    else {

        # Not going to record a transaction
        $RT::Handle->Commit() if $args{'_Commit'};
        $ErrStr = $self->loc( "Asset [_1] created of type '[_2]'", $self->Id, $TypeObj->Name );
        $ErrStr = join( "\n", $ErrStr, @non_fatal_errors );
        return ( $self->Id, $0, $ErrStr );

    }
}

sub _AddLinksOnCreateOrUpdate {
    my ($self, %args) = @_;

    my @errors;

    #Add links
    foreach my $type ( keys %LINKTYPEMAP ) {
        next unless ( defined $args{$type} );
        foreach my $link (
            ref( $args{$type} ) ? @{ $args{$type} } : ( $args{$type} ) )
        {
            # Check rights on the other end of the link if we must
            # then run _AddLink that doesn't check for ACLs
            if ( RT->Config->Get( 'StrictLinkACL' ) ) {
                my ($val, $msg, $obj) = $self->__GetAssetFromURI( URI => $link );
                unless ( $val ) {
                    push @errors, $msg;
                    next;
                }
                if ( $obj && !$obj->CurrentUserHasRight('ModifyAsset') ) {
                    push @errors, $self->loc('Linking. Permission denied');
                    next;
                }
            }

            my ( $wval, $wmsg ) = $self->_AddLink(
                Type                          => $LINKTYPEMAP{$type}->{'Type'},
                $LINKTYPEMAP{$type}->{'Mode'} => $link,
                Silent                        => 1,
                'Silent'. ( $LINKTYPEMAP{$type}->{'Mode'} eq 'Base'? 'Target': 'Base' )
                                              => 1,
            );

            push @errors, $wmsg unless ($wval);
        }
    }

    return (@errors ? 0 : 1), \@errors;
}

# }}}


sub UpdateAsset {
    my $self = shift;

    my %args = (
        id                 => undef,
        Type               => undef,
        Name               => undef,
        Description        => undef,
        Status             => undef,
        TransactionData    => undef,
        _RecordTransaction => 1,
        _Commit            => 1,
        _Detailed          => 0,
        @_
    );

    unless ($args{_Commit}) {
        $args{_RecordTransaction} = 0;
    }

    return 0, 0, $self->loc('Permission denied') unless $self->CurrentUserHasRight('ModifyAsset');

    my $changing_type = undef;
    my $changing_name = undef;
    my $changing_status = undef;
    my $asset_updated = undef;


    # If the Type is being updated make sure we can find the new Type and have CreateAsset on it
    my $TypeObj = RTx::AssetTracker::Type->new($RT::SystemUser);
    if ( defined( $args{Type} ) && $args{Type} ne $self->TypeObj->Name ) {
        $changing_type = 1;
        $TypeObj->Load( $args{Type} );

        #Can't create a asset without a type.
        unless ( $TypeObj->Id ) {
            return ( 0, 0, $self->loc("Could not load asset type '[_1]'", $args{Type}) );
        }
    
        #Now that we have a type, Check the ACLS
        unless ( $self->CurrentUser->HasRight(
                Right  => 'CreateAsset',
                Object => $TypeObj,
                EquivObjects => [ $RTx::AssetTracker::System ],)
        ) {
            return (
                0, 0,
                $self->loc( "No permission to create assets of this type '[_1]'", $TypeObj->Name));
        }
    }

    if (defined( $args{Name} ) && $args{Name} ne $self->Name ) {
        $changing_name = 1;
    }

    if (defined( $args{Status} ) && $args{Status} ne $self->Status ) {
        $changing_status = 1;
    }

    my $name   = defined $args{Name}   ? $args{Name}   : $self->Name;
    my $status = defined $args{Status} ? $args{Status} : $self->Status;

    # test name uniqueness
    if ($changing_type || $changing_name || $changing_status) {
        $asset_updated++;
        my ($rv, $msg) = $self->SatisfiesUniqueness($name, $TypeObj->Id, $status);
        unless ($rv) {
            warn ($rv, 0, $msg);
            return ($rv, 0, $msg);
        }
    }

    $RT::Handle->BeginTransaction() if $args{_Commit};

    $self->SetType( Value => $TypeObj->Id, RecordTransaction => $args{_Detailed}) if $changing_type;
    $self->SetName( Value => $name, RecordTransaction => $args{_Detailed}) if $changing_name;

    if (defined $args{Description} && $args{Description} ne $self->Description) {
        $self->SetDescription( Value => $args{Description}, RecordTransaction => $args{_Detailed});
        $asset_updated++;
    }

    if (defined($args{Status}) && $args{Status} ne $self->Status) {
        unless ( $TypeObj->IsValidStatus( $args{Status} ) ) {
            return ( 0, 0, $self->loc('Invalid value for status') );
        }
        $self->SetStatus( Value => $args{Status}, RecordTransaction => $args{_Detailed});
        $asset_updated++;
    }

    #watchers
    foreach my $type ( RTx::AssetTracker::Type->ActiveRoleArray() ) {
        next unless ( exists $args{$type} );

        my $role_method = $type . 'RoleGroup';
        my $members_current = $self->$role_method->MembersObj->ItemsArrayRef;
        my $role_current = [ map { $_->MemberObj->IsGroup ? '@'. $_->MemberObj->Object->Name()
                                                          : $_->MemberObj->Object->EmailAddress } @$members_current ];

        my $role_new = ref( $args{$type} ) ? $args{$type} : [ $args{$type} ];
        my ($add_role, $delete_role) = $self->_set_compare($role_current, $role_new);
        $asset_updated++ if @$add_role || @$delete_role;

        for my $watcher (@$delete_role) {
            next unless $watcher;

            my $field = 'Email';
            if ($watcher =~ /^\d+$/) {
                $field = 'PrincipalId';
            }
            elsif (my ($group_name) = $watcher =~ /^@(.+)$/) {
                my $group = RT::Group->new($self->CurrentUser);
                $group->LoadUserDefinedGroup($group_name);
                if ($group->Id) {
                    $field = 'PrincipalId';
                    $watcher = $group->PrincipalId;
                }
            }

            my ( $wval, $wmsg ) = $self->DeleteWatcher(
                Type   => $type,
                $field => $watcher,
                Silent => !$args{_Detailed},
            );

            return $wval, $wmsg unless ($wval);
        }

        foreach my $watcher ( @$add_role ) {
            next unless $watcher;

            my $field = 'Email';
            if ($watcher =~ /^\d+$/) {
                $field = 'PrincipalId';
            }
            elsif (my ($group_name) = $watcher =~ /^@(.+)$/) {
                my $group = RT::Group->new($self->CurrentUser);
                $group->LoadUserDefinedGroup($group_name);
                if ($group->Id) {
                    $field = 'PrincipalId';
                    $watcher = $group->PrincipalId;
                }
            }

            my ( $wval, $wmsg ) = $self->_AddWatcher(
                Type   => $type,
                $field => $watcher,
                Silent => !$args{_Detailed},
            );

            return $wval, $wmsg unless ($wval);
        }
    }

    #custom fields
    foreach my $arg ( keys %args ) {
        next unless ( $arg =~ /^CustomField-(\d+)$/i );
        my $cfid = $1;

        my $values_new =  ref( $args{$arg} ) eq 'ARRAY' ? $args{$arg} : [ $args{$arg} ];
        my $values_current = [  map { $_->Content } @{ $self->CustomFieldValues->ItemsArrayRef }];

        my ($add_values, $delete_values) = $self->_set_compare($values_current, $values_new);
        $asset_updated++ if @$add_values || @$delete_values;

        #delete old values
        for my $val (@$delete_values) {

            my $cf = RT::CustomField->new($self->CurrentUser);
            $cf->Load($cfid);

            my ( $rv, $msg ) = $cf->DeleteValueForObject(
                Object  => $self,
                Content => $val,
            );
            return 0, $msg unless $rv;

            if ( $args{_Detailed} ) {
                my ( $TransactionId, $Msg, $TransactionObj ) = $self->_NewTransaction(
                    Type          => 'CustomField',
                    Field         => $cf->Id,
                    OldReference  => $val,
                    ReferenceType => 'RT::ObjectCustomFieldValue',);
            }
        }

        #add new values
        for my $value ( @$add_values ) {
            next unless ( defined($value) && length($value) );

            # Allow passing in uploaded LargeContent etc by hash reference
            $self->_AddCustomFieldValue(
                (UNIVERSAL::isa( $value => 'HASH' )
                    ? %$value
                    : (Value => $value)
                ),
                Field             => $cfid,
                RecordTransaction => $args{_Detailed},
            );
        }
    }

    #TODO support link transactions
    my $base_links = RT::Links->new($self->CurrentUser);
    $base_links->Limit( FIELD => 'Base', VALUE => $self->URI );
    $_->Delete for @{ $base_links->ItemsArrayRef };

    my $target_links = RT::Links->new($self->CurrentUser);
    $target_links->Limit( FIELD => 'Target', VALUE => $self->URI );
    $_->Delete for @{ $target_links->ItemsArrayRef };

    my ($addlink_rv, $addlink_errors) = $self->_AddLinksOnCreateOrUpdate(%args);
    return 0, join("\n", @$addlink_errors) unless $addlink_rv;

    #TODO support IP transactions
    #Delete existing IPs
    $_->Delete for @{$self->IPs->ItemsArrayRef};

    #Add IP addresses
    my $ips = defined $args{'IP Address'} ? $args{'IP Address'} : [];
    for my $ip (@$ips) {
        my $ip_ref = {};
        if (!ref $ip) {
            $ip = { IP => $ip };
        }

        my ($rv, $msg) = $self->AddIP(%$ip, Silent => 1, SilentPorts => 1);
        return (0,0,$msg) unless ($rv);
    }


    #unless ($asset_updated) {
        #return ($self->Id, 0, $self->loc('Asset not changed'));
    #}

    my $trans_id = 0;
    if ( $args{_RecordTransaction} ) {

        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
                                     Type      => "Update",
                                     Data      => $self->loc('Asset updated on import'),
        );
        $trans_id = $TransObj->Id;
    }

    if ($args{_Commit}) {
        $RT::Handle->Commit();
    }

    return $self->Id, $trans_id, $self->loc('Asset [_1] updated', $self->Id);
}



# {{{ Routines dealing with watchers.

# {{{ _CreateAssetGroups

=head2 _CreateAssetGroups

Create the asset groups and links for this asset.
This routine expects to be called from Asset->Create _inside of a transaction_

It will create two groups for this asset: Admin and Owner.

It will return true on success and undef on failure.

=begin testing

ok(my $todd = RT::User->new($RT::SystemUser), "Creating a todd rt::user");
$todd->LoadOrCreateByEmail('todd@example.com');
ok($todd->Id,  "Found the todd rt user");

ok(my $bob = RT::User->new($RT::SystemUser), "Creating a bob rt::user");
$bob->LoadOrCreateByEmail('bob@example.com');
ok($bob->Id,  "Found the bob rt user");

my $asset = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($id, $msg) = $asset->Create(Description => "Asset Foo",
                Name => "Foo $$",
                Owner => [ $todd->Id, $RT::SystemUser->Id ],
                Status => 'production',
                Admin => $todd->Id,
                Type => '1'
                );
ok ($id, "Asset $id was created");
ok( my $group = $asset->LoadAssetRoleGroup(Type=> 'Admin'));
ok ($group->Id, "Found the requestors object for this asset");


ok ($asset->IsWatcher(Type => 'Admin', PrincipalId => $todd->PrincipalId), "The asset actually has todd at example.com as a admin");
my ($add_id, $add_msg) = $asset->AddWatcher(Type => 'Owner', Email => 'bob@example.com');
ok ($add_id, "Add bob\@example.com as Owner succeeded: ($add_msg)");

ok ($asset->IsWatcher(Type => 'Owner', PrincipalId => $bob->PrincipalId), "The asset actually has bob at example.com as a owner");;
my ($add_id, $add_msg) = $asset->DeleteWatcher(Type =>'Owner', Email => 'bob@example.com');
ok (!$asset->IsWatcher(Type => 'Owner', Principal => $bob->PrincipalId), "The asset no longer has bob at example.com as a owner");;


ok( $group = $asset->LoadAssetRoleGroup(Type=> 'Owner'));
ok ($group->Id, "Found the owner object for this asset");
ok($group->HasMember($RT::SystemUser->UserObj->PrincipalObj), "the owner group has the member 'RT_System'");
ok($group = $asset->LoadAssetRoleGroup(Type=> 'Admin'));
ok ($group->Id, "Found the Admin object for this asset");

=end testing

=cut


sub _CreateAssetGroups {
    my $self = shift;
    my %args = @_;

    foreach my $type ( RTx::AssetTracker::Type->ActiveRoleArray() ) {
        my $type_obj = RT::Group->new($self->CurrentUser);
        my ($id, $msg) = $type_obj->_Create(Domain => 'RTx::AssetTracker::Asset-Role',
                                                       Instance => $self->Id,
                                                       Type => $type,
                                                       InsideTransaction => 1,
                                                       '_RecordTransaction' => $args{'_Commit'} && $args{'_RecordTransaction'} );
        unless ($id) {
            $RT::Logger->error("Couldn't create a asset group of type '$type' for asset ".
                               $self->Id.": ".$msg);
            return(undef);
        }
     }
    return(1);

}


# }}}

# {{{ sub OwnerGroup

=head2 OwnerGroup

A constructor which returns an RT::Group object containing the owner of this asset.

This method is for backwards compatability. For each defined role there is a
method called <method_name>RoleGroup().

=cut

sub OwnerGroup {
    my $self = shift;
    return $self->OwnerRoleGroup(@_);
}

# }}}

# {{{ sub AddWatcher

=head2 AddWatcher

AddWatcher takes a parameter hash. The keys are as follows:

Type        One of Admin

PrinicpalId The RT::Principal id of the user or group that's being added as a watcher

Email       The email address of the new watcher. If a user with this
            email address can't be found, a new nonprivileged user will be created.

If the watcher you\'re trying to set has an RT account, set the Owner paremeter to their User Id. Otherwise, set the Email parameter to their Email address.

=cut

sub AddWatcher {
    my $self = shift;
    my %args = (
        Type  => undef,
        PrincipalId => undef,
        Email => undef,
        @_
    );


    # {{{ Check ACLS
    #If the watcher we're trying to add is for the current user
    if ( $args{'PrincipalId'} && $self->CurrentUser->PrincipalId == $args{'PrincipalId'}) {
        #  If it's an Admin and they don't have
        #   'WatchAsAdmin' or 'ModifyAsset', bail
        if ( $args{'Type'} ) {
            unless ( $self->CurrentUserHasRight('ModifyAsset')
                or $self->CurrentUserHasRight(RTx::AssetTracker::Type->RoleRight($args{'Type'})) ) {
                return ( 0, $self->loc('Permission Denied'))
            }
        }

        else {
            $RT::Logger->warning( "$self -> AddWatcher got passed a bogus type");
            return ( 0, $self->loc('Error in parameters to Asset->AddWatcher') );
        }
    }

    # If the watcher isn't the current user
    # and the current user  doesn't have 'ModifyAsset'
    # bail
    else {
        unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
            return ( 0, $self->loc("Permission Denied") );
        }
    }

    # }}}

    return ( $self->_AddWatcher(%args) );
}

#This contains the meat of AddWatcher. but can be called from a routine like
# Create, which doesn't need the additional acl check
sub _AddWatcher {
    my $self = shift;
    my %args = (
        Type   => undef,
        Silent => undef,
        PrincipalId => undef,
        Email => undef,
        Name  => undef,
        TransactionData => undef,
        @_
    );


    my $principal = RT::Principal->new($self->CurrentUser);
    if ($args{'Email'}) {
        my $user = RT::User->new($RT::SystemUser);
        my ($pid, $msg) = $user->LoadByEmail($args{'Email'});
        if ($pid) {
            $args{'PrincipalId'} = $user->PrincipalId;
        }
        else {
            ($pid, $msg) = $user->Load($args{'Email'});
            if ($pid) {
                $args{'PrincipalId'} = $user->PrincipalId;
            }
        }
    }
    elsif ($args{'Name'}) {
        my $user = RT::User->new($RT::SystemUser);
        my ($pid, $msg) = $user->Load($args{'Name'});
        if ($pid) {
            $args{'PrincipalId'} = $user->PrincipalId;
        }
    }
    if ($args{'PrincipalId'}) {
        $principal->Load($args{'PrincipalId'});
    }


    # If we can't find this watcher, we need to bail.
    unless ($principal->Id) {
            $RT::Logger->error("Could not load a user with the email address '".$args{'Email'}. "' to add as a watcher for asset ".$self->Id);
        return(0, $self->loc("Could not find that user"));
    }


    my $group = $self->LoadAssetRoleGroup(Type => $args{'Type'});
    unless ($group->id) {
        return(0,$self->loc("Group not found"));
    }

    if ( $group->HasMember( $principal)) {

        return ( 0, $self->loc('That principal is already a [_1] for this asset', $self->loc($args{'Type'})) );
    }


    my ( $m_id, $m_msg ) = $group->_AddMember( PrincipalId => $principal->Id,
                                               InsideTransaction => 1 );
    unless ($m_id) {
        $RT::Logger->error("Failed to add ".$principal->Id." as a member of group ".$group->Id."\n".$m_msg);

        return ( 0, $self->loc('Could not make that principal a [_1] for this asset', $self->loc($args{'Type'})) );
    }

    unless ( $args{'Silent'} ) {
        $self->_NewTransaction(
            Type     => 'AddWatcher',
            NewValue => $principal->Id,
            Field    => $args{'Type'},
            Data     => $args{TransactionData},
        );
    }

        return ( 1, $self->loc('Added principal as a [_1] for this asset', $self->loc($args{'Type'})) );
}

# }}}

# {{{ sub DeleteWatcher

=head2 DeleteWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID, Email => EMAIL_ADDRESS }


Deletes a Asset watcher.  Takes two arguments:

Type  (one of Admin)

and one of

PrincipalId (an RT::Principal Id of the watcher you want to remove)
    OR
Email (the email address of an existing wathcer)


=cut


sub DeleteWatcher {
    my $self = shift;

    my %args = ( Type        => undef,
                 PrincipalId => undef,
                 Email       => undef,
                 Name        => undef,
                 TransactionData => undef,
                 @_ );

    unless ( $args{'PrincipalId'} || $args{'Email'} ) {
        return ( 0, $self->loc("No principal specified") );
    }
    my $principal = RT::Principal->new( $self->CurrentUser );
    if ( $args{'PrincipalId'} ) {

        $principal->Load( $args{'PrincipalId'} );
    }
    elsif ($args{'Email'}) {
        my $user = RT::User->new($RT::SystemUser);
        my ($pid, $msg) = $user->LoadByEmail($args{'Email'});
        if ($pid) {
            $principal->Load($user->PrincipalId);
        }
        else {
            ($pid, $msg) = $user->Load($args{'Email'});
            if ($pid) {
                $principal->Load($user->PrincipalId);
            }
        }
    }
    elsif ($args{'Name'}) {
        my $user = RT::User->new($RT::SystemUser);
        my ($pid, $msg) = $user->Load($args{'Name'});
        if ($pid) {
            $principal->Load($user->PrincipalId);
        }
    }

    # If we can't find this watcher, we need to bail.
    unless ( $principal->Id ) {
        return ( 0, $self->loc("Could not find that principal") );
    }

    my $group = $self->LoadAssetRoleGroup( Type => $args{'Type'} );
    unless ( $group->id ) {
        return ( 0, $self->loc("Group not found") );
    }

    # {{{ Check ACLS
    #If the watcher we're trying to add is for the current user
    if ( $self->CurrentUser->PrincipalId == $principal->id ) {

        #  If it's an Admin and they don't have
        #   'WatchAsAdmin' or 'ModifyAsset', bail
        if ( $args{'Type'} ) {
            unless (    $self->CurrentUserHasRight('ModifyAsset')
                     or $self->CurrentUserHasRight(RTx::AssetTracker::Type->RoleRight($args{'Type'})) ) {
                return ( 0, $self->loc('Permission Denied') );
            }
        }

        else {
            $RT::Logger->warn("$self -> DeleteWatcher got passed a bogus type");
            return ( 0,
                     $self->loc('Error in parameters to Asset->DeleteWatcher') );
        }
    }

    # If the watcher isn't the current user
    # and the current user  doesn't have 'ModifyAsset' bail
    else {
        unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
            return ( 0, $self->loc("Permission Denied") );
        }
    }

    # }}}

    # see if this user is already a watcher.

    unless ( $group->HasMember($principal) ) {
        return ( 0,
                 $self->loc( 'That principal is not a [_1] for this asset',
                             $args{'Type'} ) );
    }

    my ( $m_id, $m_msg ) = $group->_DeleteMember( $principal->Id );
    unless ($m_id) {
        $RT::Logger->error( "Failed to delete "
                            . $principal->Id
                            . " as a member of group "
                            . $group->Id . "\n"
                            . $m_msg );

        return (0,
                $self->loc(
                    'Could not remove that principal as a [_1] for this asset',
                    $args{'Type'} ) );
    }

    unless ( $args{'Silent'} ) {
        $self->_NewTransaction( Type     => 'DelWatcher',
                                OldValue => $principal->Id,
                                Field    => $args{'Type'},
                                Data     => $args{TransactionData} );
    }

    return ( 1,
             $self->loc( "[_1] is no longer a [_2] for this asset.",
                         $principal->Object->Name,
                         $args{'Type'} ) );
}

sub DeleteAllWatchers {
    my $self = shift;
    my %args = ( Type        => undef,
                 TransactionData => undef,
                 @_ );

    my $group = $self->LoadAssetRoleGroup( Type => $args{Type} );
    unless ( $group->id ) {
        return ( 0, $self->loc("Group [_1] not found", $args{Type}) );
    }

    my $members = $group->MembersObj;
    while (my $member = $members->Next) {
        my $principal = $member->MemberObj;
        my ( $m_id, $m_msg ) = $group->_DeleteMember( $principal->Id );
        unless ($m_id) {
            $RT::Logger->error( "Failed to delete "
                                . $principal->Id
                                . " as a member of group "
                                . $group->Id . "\n"
                                . $m_msg );

            return (0,
                    $self->loc(
                        'Could not remove that principal as a [_1] for this asset',
                        $args{Type} ) );
        }

        unless ( $args{Silent} ) {
            $self->_NewTransaction( Type     => 'DelWatcher',
                                    OldValue => $principal->Id,
                                    Field    => $args{Type},
                                    Data     => $args{TransactionData} );
        }
        
    }
}



# }}}

# {{{ Routines dealing with ACCESS CONTROL

# {{{ sub CurrentUserHasRight

=head2 CurrentUserHasRight

  Takes the textual name of a Asset scoped right (from RT::ACE) and returns
1 if the user has that right. It returns 0 if the user doesn't have that right.

=cut

sub CurrentUserHasRight {
    my $self  = shift;
    my $right = shift;

    return (
        $self->HasRight(
            Principal => $self->CurrentUser->UserObj(),
            Right     => "$right",
            EquivObjects => [ $RTx::AssetTracker::System, $self->TypeObj ],
          )
    );

    unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }
}

# }}}

=head2 CurrentUserCanSee

Returns true if the current user can see the ticket, using ShowTicket

=cut

sub CurrentUserCanSee {
    my $self = shift;
    return $self->CurrentUserHasRight('ShowTicket');
}

# {{{ sub HasRight

=head2 HasRight

 Takes a paramhash with the attributes 'Right' and 'Principal'
  'Right' is a asset-scoped textual right from RT::ACE
  'Principal' is an RT::User object

  Returns 1 if the principal has the right. Returns undef if not.

=cut

sub HasRight {
    my $self = shift;
    my %args = (
        Right     => undef,
        Principal => undef,
        @_
    );

    unless ( ( defined $args{'Principal'} ) and ( ref( $args{'Principal'} ) ) )
    {
        Carp::cluck;
        $RT::Logger->crit("Principal attrib undefined for Asset::HasRight");
        return(undef);
    }

    return (
        $args{'Principal'}->HasRight(
            Object => $self,
            Right     => $args{'Right'},
            @_
          )
    );
}

# }}}

# }}}

# {{{ sub TypeObj

=head2 TypeObj

Takes nothing. returns this asset's type object

=cut

sub TypeObj {
    my $self = shift;

    my $type_obj = RTx::AssetTracker::Type->new( $self->CurrentUser );

    #We call __Value so that we can avoid the ACL decision and some deep recursion
    my ($result) = $type_obj->Load( $self->__Value('Type') );
    return ($type_obj);
}

sub AssetTypeObj { $_[0]->TypeObj() }

# }}}


# {{{ sub AdminGroup

=head2 AdminGroup

Takes nothing.
Returns an RT::Group object which contains this asset's Admins.
If the user doesn't have "ShowAsset" permission, returns an empty group

This method is for backwards compatability. For each defined role there is a
method called <method_name>RoleGroup().

=cut

sub AdminGroup {
    my $self = shift;
    return $self->AdminRoleGroup(@_);
}

# }}}

# {{{ IsWatcher,IsOwner,IsAdmin

# {{{ sub IsWatcher
# a generic routine to be called by IsOwner and IsAdmin

=head2 IsWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID, Email => EMAIL }

Takes a param hash with the attributes Type and either PrincipalId or Email

Type is one of Owner or Admin

PrincipalId is an RT::Principal id, and Email is an email address.

Returns true if the specified principal (or the one corresponding to the
specified address) is a member of the group Type for this asset.

XX TODO: This should be Memoized.

=cut

sub IsWatcher {
    my $self = shift;

    my %args = ( Type  => 'Owner',
        PrincipalId    => undef,
        Email          => undef,
        @_
    );

    # Load the relevant group.
    my $group = $self->LoadAssetRoleGroup(Type => $args{'Type'});

    # Find the relevant principal.
    my $principal = RT::Principal->new($self->CurrentUser);
    if (!$args{PrincipalId} && $args{Email}) {
        # Look up the specified user.
        my $user = RT::User->new($self->CurrentUser);
        $user->LoadByEmail($args{Email});
        if ($user->Id) {
            $args{PrincipalId} = $user->PrincipalId;
        }
        else {
            # A non-existent user can't be a group member.
            return 0;
        }
    }
    $principal->Load($args{'PrincipalId'});

    # Ask if it has the member in question
    return ($group->HasMember($principal));
}

# }}}

# {{{ sub IsOwner

=head2 IsOwner PRINCIPAL_ID

  Takes an RT::Principal id
  Returns true if the principal is a owner of the current asset.

  This method is auto-generated.

=cut

# }}}

# {{{ sub IsAdmin

=head2 IsAdmin PRINCIPAL_ID

  Takes an RT::Principal id.
  Returns true if the principal is a requestor of the current asset.

  This method is auto-generated.

=cut

# }}}

# }}}


sub CustomFieldLookupType {
    "RTx::AssetTracker::Type-RTx::AssetTracker::Asset";
}

# for pre RT 3.4.2 compatability
sub _LookupTypes {
    "RTx::AssetTracker::Type-RTx::AssetTracker::Asset";
}

# {{{ sub _Set

sub _Set {
    my $self = shift;

    my %args = ( Field             => undef,
                 Value             => undef,
                 TimeTaken         => 0,
                 RecordTransaction => 1,
                 UpdateAsset      => 1,
                 CheckACL          => 1,
                 TransactionType   => 'Set',
                 TransactionData   => undef,
                 @_ );

    if ($args{'CheckACL'}) {
      unless ( $self->CurrentUserHasRight('ModifyAsset')) {
          return ( 0, $self->loc("Permission Denied"));
      }
   }

    unless ($args{'UpdateAsset'} || $args{'RecordTransaction'}) {
        $RT::Logger->error("Asset->_Set called without a mandate to record an update or update the asset");
        return(0, $self->loc("Internal Error"));
    }

    #if the user is trying to modify the record

    #Take care of the old value we really don't want to get in an ACL loop.
    # so ask the super::_Value
    my $Old = $self->SUPER::_Value("$args{'Field'}");

    my ($ret, $msg);
    if ( $args{'UpdateAsset'}  ) {

        #Set the new value
        ( $ret, $msg ) = $self->SUPER::_Set( Field => $args{'Field'},
                                                Value => $args{'Value'} );

        #If we can't actually set the field to the value, don't record
        # a transaction. instead, get out of here.
        if ( $ret == 0 ) { return ( 0, $msg ); }
    }

    if ( $args{'RecordTransaction'} == 1 ) {

        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
                                               Type => $args{'TransactionType'},
                                               Field     => $args{'Field'},
                                               NewValue  => $args{'Value'},
                                               OldValue  => $Old,
                                               #TimeTaken => $args{'TimeTaken'},
                                               TimeTaken => undef,
                                               Data      => $args{TransactionData},
        );
        return ( $Trans, scalar $TransObj->Description );
    }
    else {
        return ( $ret, $msg );
    }
}

# }}}

# {{{ sub _Value

=head2 _Value

Takes the name of a table column.
Returns its value as a string, if the user passes an ACL check

=cut

sub _Value {

    my $self  = shift;
    my $field = shift;

    #if the field is public, return it.
    if ( $self->_Accessible( $field, 'public' ) ) {

        #$RT::Logger->debug("Skipping ACL check for $field\n");
        return ( $self->SUPER::_Value($field) );

    }

    #If the current user doesn't have ACLs, don't let em at it.

    unless ( $self->CurrentUserHasRight('ShowAsset') ) {
        return (undef);
    }
    return ( $self->SUPER::_Value($field) );

}

# }}}

# {{{ sub CustomFieldValues

=head2 CustomFieldValues

# Do name => id mapping (if needed) before falling back to
# RT::Record's CustomFieldValues

See L<RT::Record>

=cut

sub CustomFieldValues {
    my $self  = shift;
    my $field = shift;
    if ( $field and $field !~ /^\d+$/ ) {
        my $cf = RT::CustomField->new( $self->CurrentUser );
        $cf->SetContextObject( $self );
        $cf->LoadByName( Name => $field, Type => $self->TypeObj->Id );
        unless ( $cf->id ) {
            $cf->LoadByName( Name => $field, Type => '0' );
        }
        $field = $cf->id;
        unless ($field) { return RT::CustomFieldValues->new( $self->CurrentUser ); }
    }
    return $self->SUPER::CustomFieldValues($field);
}

# }}}

sub IPs {
    my $self = shift;
    my $ips = RTx::AssetTracker::IPs->new( $self->CurrentUser );
    $ips->Limit( FIELD => 'Asset', VALUE => $self->Id );

    return $ips;
}


sub DeleteIP {

    my $self = shift;
    my %args = ( IP        => undef,
                 TransactionData => undef,
                 @_ );

    unless ( $self->CurrentUserHasRight('ModifyAsset') || $self->CurrentUserHasRight('RetireAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    my $ip = RTx::AssetTracker::IP->new( $self->CurrentUser );
    $ip->Load( $args{IP} );
    my $addr = $ip->IP;

    # If we can't find this IP, we need to bail.
    unless ( $ip->Id ) {
        return ( 0, $self->loc("Could not find that IP") );
    }

    my ($rv, $msg) = $ip->DeleteAllPorts();
    unless ($rv) {
        return($rv, "IP address could not be deleted: $msg");
    }

    my $retval = $ip->Delete();
    if ($retval) {
        unless ( $args{'Silent'} ) {
            $self->_NewTransaction( Type     => 'DelIP',
                                    OldValue => $addr,
                                    Field    => 'IP',
                                    Data     => $args{TransactionData} );
        }

        return ( 1, $self->loc( "$addr is no longer an IP for this asset." ) );
    } else {
        return(0, $self->loc("IP address could not be deleted"));
    }

}

sub AddIP {

    my $self = shift;
    my %args = ( IP        => undef,
                 Interface => undef,
                 MAC => undef,
                 TCPPorts => [],
                 UDPPorts => [],
                 Silent => 0,
                 SilentPorts => 0,
                 TransactionData => undef,
                 @_ );

    unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    if ( $self->Status eq 'retired') {
        return ( 0, $self->loc("Retired assets cannot have IP addresses") );
    }

    if ( $self->Status eq 'deleted') {
        return ( 0, $self->loc("Deleted assets cannot have IP addresses") );
    }

    unless ($args{IP} or $args{Interface}) {
        return ( 0, $self->loc("IP address or interface must be specified") );
    }

    my $ip = RTx::AssetTracker::IP->new( $self->CurrentUser );
    my ($rc, $msg) = $ip->Create( IP => $args{IP}, Interface => $args{Interface}, MAC => $args{MAC}, Asset => $self->Id, TCPPorts => $args{TCPPorts}, UDPPorts => $args{UDPPorts}, Silent => $args{Silent}, SilentPorts => $args{SilentPorts} );

    if ($ip->Id) {
        unless ( $args{'Silent'} ) {
            $self->_NewTransaction( Type     => 'AddIP',
                                    NewValue => $args{IP},
                                    Field    => 'IP',
                                    Data     => $args{TransactionData} );
        }
        return ( $ip->Id, $self->loc( "$args{IP} is now an IP for this asset." ) );
    } else {
        return( 0, $self->loc("IP address could not be created: $msg"));
    }

}

sub _export_formatted_IPs {
    my ($self) = @_;

    my $ips = $self->IPs->ItemsArrayRef;
    return join('|', map { $self->_format_IP($_) } @$ips);
}

sub _format_IP {
    my ($self, $ip) = @_;

    return join(':', $ip->Interface, $ip->IP, $ip->MAC, join(',', $ip->TCPPorts), join(',', $ip->UDPPorts));
}

sub IPsAsList {

    my $self = shift;
    my $ips = $self->IPs;
    my @ips;
    while (my $ip = $ips->Next) {
        push @ips, $ip->IP;
    }

    return @ips;

}

sub IPsAsString {

    my $self = shift;

    return join(',', $self->IPsAsList);
}

sub ComponentsAsList {

    return ('Asset::ComponentsAsList no longer supported');

}

# {{{ sub SetStatus

=head2 SetStatus STATUS

Set this asset\'s status. STATUS can be one of: 

Alternatively, you can pass in a list of named parameters (Status => STATUS, Force => FORCE).  If FORCE is true, ignore unresolved dependencies and force a status change.

=begin testing

my $tt = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($id, $tid, $msg)= $tt->Create(Type => 'Servers', Name => "SetStatus test $$",
            Description => 'test');
ok($id, $msg);
is($tt->Status, 'production', "New asset is created as production");

($id, $msg) = $tt->SetStatus('development');
ok($id, $msg);
like($msg, qr/development/i, "Status message is correct");
($id, $msg) = $tt->SetStatus('qa');
ok($id, $msg);
like($msg, qr/qa/i, "Status message is correct");
($id, $msg) = $tt->SetStatus('qa');
ok(!$id,$msg);


=end testing


=cut

sub SetStatus {
    my $self   = shift;
    my %args;

    if (@_ == 1) {
        $args{Status} = shift;
    }
    else {
        %args = (@_);
    }

    $args{Status} = $args{Status} || $args{Value};

    #Check ACL
    if ( $args{Status} eq 'deleted') {
            unless ($self->CurrentUserHasRight('DeleteAsset')) {
            return ( 0, $self->loc('Permission Denied') );
       }
        # We don't want deleted assets to have IP addresses
        my $ips = $self->IPs();
        while (my $ip = $ips->Next) {
            my($a, $b) = $self->DeleteIP( IP => $ip->IP );
        }
    } elsif ( $args{Status} eq 'retired') {
            unless ($self->CurrentUserHasRight('RetireAsset')) {
            return ( 0, $self->loc('Permission Denied') );
       }
        # We don't want retired assets to have IP addresses
        my $ips = $self->IPs();
        while (my $ip = $ips->Next) {
            my($a, $b) = $self->DeleteIP( IP => $ip->IP );
        }
    } else {
            unless ($self->CurrentUserHasRight('ModifyAsset')) {
            return ( 0, $self->loc('Permission Denied') );
       }
    }

    unless (grep {$_ eq $args{'Status'}} (@RT::AssetActiveStatus,@RT::AssetInactiveStatus)) {
      return (0, $self->loc('[_1] is not a valid Asset Status', $args{'Status'})
);
    }

    #Actually update the status
    my ($val, $msg)= $self->_Set( %args,
                          Field           => 'Status',
                          Value           => $args{Status},
                          TimeTaken       => 0,
                          TransactionType => 'Status', );

    return($val,$msg);
}

# }}}

# {{{ sub _Links

sub _Links {
    my $self = shift;

    #TODO: Field isn't the right thing here. but I ahave no idea what mnemonic ---
    #tobias meant by $f
    my $field = shift;
    my $type  = shift || "";

    unless ( $self->{"$field$type"} ) {
        $self->{"$field$type"} = new RT::Links( $self->CurrentUser );
        if ( $self->CurrentUserHasRight('ShowAsset') ) {
            # at least to myself
            $self->{"$field$type"}->Limit( FIELD => $field,
                                           VALUE => $self->URI );
            $self->{"$field$type"}->Limit( FIELD => 'Type',
                                           VALUE => $type )
              if ($type);
        }
    }
    return ( $self->{"$field$type"} );
}

# }}}

# {{{ sub DeleteLink

=head2 DeleteLink

Delete a link. takes a paramhash of Base, Target and Type.
Either Base or Target must be null. The null value will
be replaced with this asset\'s id

=cut

sub DeleteLink {
    my $self = shift;
    my %args = (
        Base   => undef,
        Target => undef,
        Type   => undef,
        TransactionData => undef,
        @_
    );

    #check acls
    unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
        $RT::Logger->debug("No permission to delete links\n");
        return ( 0, $self->loc('Permission Denied'))

    }

    my ($val, $Msg) = $self->SUPER::_DeleteLink(%args);

    if ( !$val ) {
        $RT::Logger->debug("Couldn't find that link\n");
        return ( 0, $Msg );
    }

    my ($direction, $remote_link);

    if ( $args{'Base'} ) {
        $remote_link = $args{'Base'};
        $direction = 'Target';
    }
    elsif ( $args{'Target'} ) {
        $remote_link = $args{'Target'};
        $direction='Base';
    }


    if ( $val ) {
        my $remote_uri = RT::URI->new( $RT::SystemUser );
        $remote_uri->FromURI( $remote_link );

        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
            Type      => 'DeleteLink',
            Field => $LINKDIRMAP{$args{'Type'}}->{$direction},
            OldValue =>  $remote_uri->URI || $remote_link,
            TimeTaken => 0,
            Data     => $args{TransactionData},
        );
        if ( $remote_uri->IsLocal ) {

            my $OtherObj = $remote_uri->Object;
            my ( $val, $Msg ) = $OtherObj->_NewTransaction(Type  => 'DeleteLink',
                                                           Field => $direction eq 'Target' ? $LINKDIRMAP{$args{'Type'}}->{Base}
                                                                                           : $LINKDIRMAP{$args{'Type'}}->{Target},
                                                           OldValue => $self->URI,
                                                           TimeTaken => 0 );
        }


        return ( $Trans, $Msg );
    }
}

# }}}

# {{{ sub AddLink

=head2 AddLink

Takes a paramhash of Type and one of Base or Target. Adds that link to this asset.

=begin testing


=end testing

=cut

sub AddLink {
    my $self = shift;
    my %args = ( Target => '',
                 Base   => '',
                 Type   => '',
                 Silent => undef,
                 TransactionData => undef,
                 @_ );


    unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }


    $self->_AddLink(%args);
}

=head2 _AddLink

Private non-acled variant of AddLink so that links can be added during create.

=cut

sub _AddLink {
    my $self = shift;
    my %args = ( Target => '',
                 Base   => '',
                 Type   => '',
                 Silent => undef,
                 @_ );

    # {{{ If the other URI is an RTx::AssetTracker::Asset, we want to make sure the user
    # can modify it too... (do we?)
    my $other_asset_uri = RT::URI->new($self->CurrentUser);

    if ( $args{'Target'} ) {
        $other_asset_uri->FromURI( $args{'Target'} );

    }
    elsif ( $args{'Base'} ) {
        $other_asset_uri->FromURI( $args{'Base'} );
    }

    if ( $other_asset_uri->Resolver->Scheme eq 'at'
     and $RT::ModifyBothAssetsForLink) {
        my $object = $other_asset_uri->Resolver->Object;

        if (   UNIVERSAL::isa( $object, 'RTx::AssetTracker::Asset' )
            && $object->id
            && !$object->CurrentUserHasRight('ModifyAsset') )
        {
            return ( 0, $self->loc("Permission Denied") );
        }

    }

    # }}}

    my ($val, $Msg) = $self->SUPER::_AddLink(%args);

    if (!$val) {
        return ($val, $Msg);
    }

    my ($direction, $remote_link);
    if ( $args{'Target'} ) {
        $remote_link  = $args{'Target'};
        $direction    = 'Base';
    } elsif ( $args{'Base'} ) {
        $remote_link  = $args{'Base'};
        $direction    = 'Target';
    }

    # Don't write the transaction if we're doing this on create
    if ( $args{'Silent'} or $Msg eq $self->loc('Link already exists') ) {
        return ( 1, $Msg );
    }
    else {
        my $remote_uri = RT::URI->new( $RT::SystemUser );
        $remote_uri->FromURI( $remote_link );

        #Write the transaction
        my ( $Trans, $TransMsg, $TransObj ) =
            $self->_NewTransaction(Type  => 'AddLink',
                                   Field => $LINKDIRMAP{$args{'Type'}}->{$direction},
                                   NewValue =>  $remote_uri->URI || $remote_link,
                                   TimeTaken => 0,
                                   Data     => $args{TransactionData}, );
        if ( $remote_uri->IsLocal ) {

            # create transaction for other object;
            my $OtherObj = $remote_uri->Object;
            my ( $val, $Msg ) = $OtherObj->_NewTransaction(Type  => 'AddLink',
                                                           Field => $direction eq 'Target' ? $LINKDIRMAP{$args{'Type'}}->{Base}
                                                                                           : $LINKDIRMAP{$args{'Type'}}->{Target},
                                                           NewValue => $self->URI,
                                                           TimeTaken => 0 );
        }
        return ( $Trans, $Msg );
    }

}

sub __GetAssetFromURI {
    my $self = shift;
    my %args = ( URI => '', @_ );

    my $uri_obj = RT::URI->new( $self->CurrentUser );
    $uri_obj->FromURI( $args{'URI'} );

    unless ( $uri_obj->Resolver && $uri_obj->Scheme ) {
        my $msg = $self->loc( "Couldn't resolve '[_1]' into a URI.", $args{'URI'} );
        $RT::Logger->warning( $msg );
        return( 0, $msg );
    }
    my $obj = $uri_obj->Resolver->Object;
    unless ( UNIVERSAL::isa($obj, 'RTx::AssetTracker::Asset') && $obj->id ) {
        return (1, 'Found not an asset', undef);
    }
    return (1, 'Found asset', $obj);
}


# }}}

sub SetName {

    my $self = shift;
    my %args = (
                  Value => undef,
        TransactionData => undef,
                              @_,
              );

    my ($rv, $msg) = $self->SatisfiesUniqueness($args{Value}, $self->Type, $self->Status);

    if ($rv) {
        return $self->_Set(%args, Field => 'Name');#, Value => $args{Value}, TransactionData => $args{TransactionData});
    }
    else {
        return $rv, $msg;
    }

}


sub SatisfiesUniqueness {

    my $self = shift;
    my $name = shift;
    my $type = shift;
    my $stat = shift;

    my $Assets = RTx::AssetTracker::Assets->new( $RT::SystemUser );
    my $Type   = RTx::AssetTracker::Type->new( $RT::SystemUser );
    $Type->Load($type);

    if ($RT::GlobalUniqueAssetName) {
        $Assets->Limit(FIELD => "Name", VALUE => $name);
        $Assets->Limit(FIELD => "id", OPERATOR => "!=", VALUE => $self->Id) if $self->Id;
        return (0, "Asset name $name isn't unique across the entire asset database") if $Assets->Count;
    }
    if ($RT::TypeUniqueAssetName) {
        $Assets->Limit(FIELD => "Type", VALUE => $type);
        $Assets->Limit(FIELD => "id", OPERATOR => "!=", VALUE => $self->Id) if $self->Id;
        return (0, "Asset name $name isn't unique among assets of type: " . $Type->Name) if $Assets->Count;
    }
    if ($RT::TypeStatusUniqueAssetName) {
        $Assets->Limit(FIELD => "Status", VALUE => $stat);
        $Assets->Limit(FIELD => "id", OPERATOR => "!=", VALUE => $self->Id) if $self->Id;
        return (0, "Asset name $name isn't unique among assets of type: " . $Type->Name . ", and status: $stat") if $Assets->Count;
    }

    return 1, "Asset name satifies uniqueness.";

}

# {{{ sub LoadAssetRoleGroup

=head2 LoadAssetRoleGroup  { Type => TYPE }

Loads a asset group from the database.

Takes a param hash with 1 parameters:

    Type is the type of Group we're trying to load:
        Requestor, Cc, AdminCc, Owner

=cut

sub LoadAssetRoleGroup {
    my $self       = shift;

    my %args = ( Type => undef,
                @_);

    my $group = RT::Group->new( $self->CurrentUser );
    $group->LoadByCols( Domain => 'RTx::AssetTracker::Asset-Role',
                           Instance =>$self->Id,
                           Type => $args{'Type'}
                           );

    # if it doesn't exits ( like when we add a new role in the config file )
    # create it
    unless ( $group->id ) {
        my ($id, $msg) = $group->_Create(Instance => $self->Id,
                                            Type => $args{Type},
                                            Domain => 'RTx::AssetTracker::Asset-Role',
                                            InsideTransaction => 0);
        unless ($id) {
            $RT::Logger->error("Couldn't create an Asset role group of type '$args{Type}' for asset ".
                               $self->Id.": ".$msg);
        }
    }

    return $group;
}

# }}}

# {{{ Link Collections

# {{{ sub Members

=head2 Members

  This returns an RT::Links object which references all the assets
which are 'MembersOf' this asset

=cut

#sub RefersTo {
#    my $self = shift;
#    return ( $self->_Links( 'Base', 'RefersTo' ) );
#}

#sub ReferredToBy {
#    my $self = shift;
#    return ( $self->_Links( 'Target', 'RefersTo' ) );
#}

#sub RunsOn {
#    my $self = shift;
#    return ( $self->_Links( 'Base', 'RunsOn' ) );
#}

#sub IsRunning {
#    my $self = shift;
#    return ( $self->_Links( 'Target', 'RunsOn' ) );
#}

#sub DependsOn {
#    my $self = shift;
#    return ( $self->_Links( 'Base', 'DependsOn' ) );
#}

#sub DependedOnBy {
#    my $self = shift;
#    return ( $self->_Links( 'Target', 'DependsOn' ) );
#}

#sub ComponentOf {
#    my $self = shift;
#    return ( $self->_Links( 'Base', 'ComponentOf' ) );
#}

sub HasComponents {
    my $self = shift;
    return ( $self->_Links( 'Target', 'ComponentOf' ) );
}

sub Components {
    my $self = shift;
    return ( $self->_Links( 'Target', 'ComponentOf' ) );
}

# }}}

sub _UpdateTimeTaken {

    return 1;

}

sub SetDescription {
    my $self = shift;
           
    $self->_SetBasic(@_, Field => 'Description');
}

sub SetType {
    my $self = shift;
           
    $self->_SetBasic(@_, Field => 'Type');
}

sub _SetBasic {
    my $self = shift;

    my $extra_value;
    if (@_ % 2 ) {

        # odd number of arguments left. first is the value. called using old-skool one arg way (no TransactionData)
        $extra_value = shift;
        $RT::Logger->crit("Deprecated call to Set*. Asset Tracker uses named parameters for Set methods");
    }

    my %args = (
           Field => undef,
           Value => undef,
           TransactionData => undef,
                    @_,
    );
    $args{Value} = $extra_value if defined $extra_value;

    if ( $self->_Accessible( $args{Field}, 'write' ) ) {

        return ( $self->_Set( Field => $args{Field}, Value => $args{Value}, TransactionData => $args{TransactionData} ) );
    }

    elsif ( $self->_Accessible( $args{Field}, 'read' ) ) {
        return ( 0, 'Immutable field' );
    }
    else {
        return ( 0, 'Nonexistant field?' );
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

# Asset role groups( Owner, Admin, etc. )
    my $objs = RT::Groups->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Domain', VALUE => 'RTx::AssetTracker::Asset-Role' );
    $objs->Limit( FIELD => 'Instance', VALUE => $self->Id );
    push( @$list, $objs );

# IP Addresses
    $objs = RTx::AssetTracker::IPs->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Asset', VALUE => $self->Id );
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

sub Export {
    my ($self) = @_;

    return $self;
}

use Set::Scalar;
sub _set_compare {
    my ($self, $current, $new) = @_;

    my $current_set = Set::Scalar->new(@$current);
    my $new_set = Set::Scalar->new(@$new);

    my $add    = $new_set - $current_set;
    my $delete = $current_set - $new_set;
    return $add, $delete;

}

1;
