=head1 NAME

  RTx::AssetTracker::Type - an AssetTracker Type object

=head1 SYNOPSIS

  use RTx::AssetTracker::Type;

=head1 DESCRIPTION


=head1 METHODS

=begin testing 

use RTx::AssetTracker::Type;

=end testing

=cut


package RTx::AssetTracker::Type;

use strict;
no warnings qw(redefine);

use RT::CustomField;
use RT::CustomFields;
use RT::Group;

use vars qw(@DEFAULT_ACTIVE_STATUS @DEFAULT_INACTIVE_STATUS $RIGHTS %DEFAULT_ROLES);

@DEFAULT_ACTIVE_STATUS = qw(production development qa pilot dr test);
@DEFAULT_INACTIVE_STATUS = qw(retired);

$RIGHTS = {
    SeeType            => 'Can this principal see this asset type',       # loc_pair
    AdminType          => 'Create, delete and modify asset types',        # loc_pair
    AssignCustomFields => 'Assign and remove custom fields',              # loc_pair
    ModifyTypeAdmins   => 'Modify administrators for type',              # loc_pair
    
    ShowAsset      => 'See asset details',                                # loc_pair
    CreateAsset    => 'Create assets of this type',                       # loc_pair
    ModifyAsset    => 'Modify assets of this type',                       # loc_pair
    RetireAsset    => 'Retire assets of this type',                       # loc_pair
#    OwnAsset       => 'Own assets of this type',                       # loc_pair
#    WatchAsAdmin   => 'Right to administer type',

};

# Tell RT::ACE that this sort of object can get acls granted
$RT::ACE::OBJECT_TYPES{'RTx::AssetTracker::Type'} = 1;

# TODO: This should be refactored out into an RT::ACLedObject or something
# stuff the rights into a hash of rights that can exist.

foreach my $right ( keys %{$RIGHTS} ) {
    $RT::ACE::LOWERCASERIGHTNAMES{ lc $right } = $right;
}

# {{{ Setup Roles/Watchers

%DEFAULT_ROLES = (
    Admin => {        Role => 'Admin',
                     Label => 'Administrators',
                     Right => 'WatchAsAdmin',
               Description => 'Right to administer asset of this type',
             },
    Owner => {        Role => 'Owner',
                     Label => 'Owners',
                     Right => 'OwnAsset',
               Description => 'Right to own asset of this type',
             },
);

sub ActiveRoleArray {
    my $self = shift;

    if (@RTx::AssetTracker::ActiveRoles) {
        return (@RTx::AssetTracker::ActiveRoles)
    } else {
        return (sort keys %DEFAULT_ROLES);
    }
}

sub RoleLabel {
    my $self = shift;
    my $role = shift;

    if (exists $DEFAULT_ROLES{$role}) {
        return $DEFAULT_ROLES{$role}{Label};
    }

    return $role;
}

sub RoleRight {
    my $self = shift;
    my $role = shift;

    if (exists $DEFAULT_ROLES{$role}) {
        return $DEFAULT_ROLES{$role}{Right};
    }

    return $role.'Role';
}

sub RoleDescription {
    my $self = shift;
    my $role = shift;

    if (exists $DEFAULT_ROLES{$role}) {
        return $DEFAULT_ROLES{$role}{Description};
    }

    return "Right to have role '$role' for this asset type";
}

sub ConfigureRoles {

    my $class = shift;

    foreach my $role ( ActiveRoleArray() ) {
        $class->ConfigureRole( $role );
    }

}

sub ConfigureRole {

    my $class = shift;
    my $role = shift;

    # if the system role group doesn't exist, create it
    my $group = RT::Group->new( $RT::SystemUser );
    $group->LoadByCols( Domain => 'RTx::AssetTracker::System-Role', Type => $role );
    unless ( $group->id ) {
        $group->_Create( Domain            => 'RTx::AssetTracker::System-Role',
                         Instance          => 0,
                         Type              => $role,
                         InsideTransaction => 0 );
        $group->id or $RT::Logger->error("Couldn't create group for system role '$role'");
    }

    my $right = RoleRight(undef, $role);
    $RIGHTS->{$right} = RoleDescription(undef, $role);
    $RT::ACE::LOWERCASERIGHTNAMES{ lc $right } = $right;

    my $group_role_method = $role . "RoleGroup";
    my $is_role_method = 'Is' . $role;

    {

        no strict 'refs';

        *$group_role_method = sub {
            my $self = shift;

            my $group;
            if ( $self->CurrentUserHasRight('SeeType') ) {
                $group = $self->LoadTypeRoleGroup(Type => $role);
            }
            return ($group);
        };

        *$is_role_method = sub {
            my $self  = shift;
            my $owner = shift;

            return ( $self->IsWatcher( Type => $role, PrincipalId => $owner ) );

        };


    }

    {

        no strict 'refs';
        package RTx::AssetTracker::Asset;

        *$group_role_method = sub {
            my $self = shift;

            my $group;
            if ( $self->CurrentUserHasRight('ShowAsset') ) {
                $group = $self->LoadAssetRoleGroup(Type => $role);
            }
            return ($group);

        };

        *$is_role_method = sub {
            my $self  = shift;
            my $owner = shift;

            return ( $self->IsWatcher( Type => $role, PrincipalId => $owner ) );

        };

    }


}


# }}}

# {{{ ActiveStatusArray

=head2 ActiveStatusArray

Returns an array of all ActiveStatuses for this queue

=cut

sub ActiveStatusArray {
    my $self = shift;
    if (@RTx::AssetTracker::ActiveStatus) {
        return (@RTx::AssetTracker::ActiveStatus)
    } else {
        $RT::Logger->warning("RTx::AssetTracker::ActiveStatus undefined, falling back to deprecated defaults");
        return (@DEFAULT_ACTIVE_STATUS);
    }
}

# }}}

# {{{ InactiveStatusArray

=head2 InactiveStatusArray

Returns an array of all InactiveStatuses for this queue

=cut

sub InactiveStatusArray {
    my $self = shift;
    if (@RTx::AssetTracker::InactiveStatus) {
        return (@RTx::AssetTracker::InactiveStatus)
    } else {
        $RT::Logger->warning("RTx::AssetTracker::InactiveStatus undefined, falling back to deprecated defaults");
        return (@DEFAULT_INACTIVE_STATUS);
    }
}

# }}}

# {{{ StatusArray

=head2 StatusArray

Returns an array of all statuses for this queue

=cut

sub StatusArray {
    my $self = shift;
    return ($self->ActiveStatusArray(), $self->InactiveStatusArray());
}

# }}}

# {{{ IsValidStatus

=head2 IsValidStatus VALUE

Returns true if VALUE is a valid status.  Otherwise, returns 0

=for testing
my $t = RTx::AssetTracker::Type->new($RT::SystemUser);
ok($t->IsValidStatus('production')== 1, 'New is a valid status');
ok($t->IsValidStatus('f00')== 0, 'f00 is not a valid status');

=cut

sub IsValidStatus {
    my $self  = shift;
    my $value = shift;

    my $retval = grep ( /^$value$/, $self->StatusArray );
    return ($retval);

}

# }}}

# {{{ IsActiveStatus

=head2 IsActiveStatus VALUE

Returns true if VALUE is a Active status.  Otherwise, returns 0

=for testing
my $t = RTx::AssetTracker::Type->new($RT::SystemUser);
ok($t->IsActiveStatus('production')== 1, 'New is a Active status');
ok($t->IsActiveStatus('retired')== 0, 'Rejected is an inactive status');
ok($t->IsActiveStatus('f00')== 0, 'f00 is not a Active status');

=cut

sub IsActiveStatus {
    my $self  = shift;
    my $value = shift;

    my $retval = grep ( /^$value$/, $self->ActiveStatusArray );
    return ($retval);

}

# }}}

# {{{ IsInactiveStatus

=head2 IsInactiveStatus VALUE

Returns true if VALUE is a Inactive status.  Otherwise, returns 0

=for testing
my $t = RTx::AssetTracker::Type->new($RT::SystemUser);
ok($t->IsInactiveStatus('production')== 0, 'New is a Active status');
ok($t->IsInactiveStatus('retired')== 1, 'rejeected is an Inactive status');
ok($t->IsInactiveStatus('f00')== 0, 'f00 is not a Active status');

=cut

sub IsInactiveStatus {
    my $self  = shift;
    my $value = shift;

    my $retval = grep ( /^$value$/, $self->InactiveStatusArray );
    return ($retval);

}

# }}}


    
=head2 AvailableRights

Returns a hash of available rights for this object. The keys are the right names and the values are a description of what the righ
ts do

=cut

sub AvailableRights {
    my $self = shift;
    return($RIGHTS);
}

# {{{ sub Create




=head2 Create

Create takes the name of the new type
If you pass the ACL check, it creates the type and returns its type id.

=begin testing

my $type = RTx::AssetTracker::Type->new($RT::SystemUser);
my ($id, $val) = $type->Create( Name => 'Test1');
ok($id, $val);

($id, $val) = $type->Create( Name => '66');
ok(!$id, $val);


=end testing


=cut

sub Create {
    my $self = shift;
    my %args = (
        Name              => undef,
        Description       => '',
        @_
    );

    unless ( $self->CurrentUser->HasRight(Right => 'AdminType', Object => $RTx::AssetTracker::System) )
    {    #Check them ACLs
        return ( 0, $self->loc("No permission to create asset types") );
    }

    unless ( $self->ValidateName( $args{'Name'} ) ) {
        return ( 0, $self->loc('Asset type already exists') );
    }

    #TODO better input validation
    $RT::Handle->BeginTransaction();

    my $id = $self->SUPER::Create(%args);
    unless ($id) {
        $RT::Handle->Rollback();
        return ( 0, $self->loc('Asset type could not be created') );
    }

    my $create_ret = $self->_CreateTypeGroups();
    unless ($create_ret) {
        $RT::Handle->Rollback();
        return ( 0, $self->loc('Asset type could not be created') );
    }


    $RT::Handle->Commit();
    return ( $id, $self->loc("Asset type created") );
}

# }}}

# {{{ sub ValidateName

=head2 ValidateName NAME

Takes a type name. Returns true if it's an ok name for
a new type. Returns undef if there's already a type by that name.

=cut

sub ValidateName {
    my $self = shift;
    my $name = shift;

    my $temptype = new RTx::AssetTracker::Type($RT::SystemUser);
    $temptype->Load($name);

    #If this queue exists, return undef
    if ( $temptype->Name() ) {
        return (undef);
    }

    #If the type doesn't exist, return 1
    else {
        return ($self->SUPER::ValidateName($name));
    }

}

# }}}

# {{{ sub Load

=head2 Load

Takes either a numerical id or a textual Name and loads the specified type.

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
        $self->LoadByCols( Name => $identifier );
    }

    return ( $self->Id );

}

# }}}

# {{{ sub Delete

sub Delete {
    my $self = shift;
    return ( 0,
        $self->loc('Deleting this object would break referential integrity') );
}

# }}}

# {{{  CustomField

=item CustomField NAME

Load the type-specific custom field named NAME

=cut

sub CustomField {
    my $self = shift;
    my $name = shift;
    my $cf = RT::CustomField->new($self->CurrentUser);
    $cf->LoadByName(Name => $name, Type => $self->Id);
    return ($cf);
}


# {{{ ACCESS CONTROL

# {{{ sub _Set
sub _Set {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('AdminType') ) {
        return ( 0, $self->loc('Permission Denied') );
    }
    return ( $self->SUPER::_Set(@_) );
}

# }}}

# {{{ sub _Value

sub _Value {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('SeeType') ) {
        return (undef);
    }

    return ( $self->__Value(@_) );
}

# }}}



# {{{ sub CurrentUserHasRight

=head2 CurrentUserHasRight

Takes one argument. A textual string with the name of the right we want to check.
Returns true if the current user has that right for this type.
Returns undef otherwise.

=cut

sub CurrentUserHasRight {
    my $self  = shift;
    my $right = shift;

    return (
        $self->HasRight(
            Principal => $self->CurrentUser,
            Right     => "$right",
            EquivObjects => [$RTx::AssetTracker::System],
          )
    );

}

# }}}

# {{{ sub HasRight

=head2 HasRight

Takes a param hash with the fields 'Right' and 'Principal'.
Principal defaults to the current user.
Returns true if the principal has that right for this type.
Returns undef otherwise.

=cut

# TAKES: Right and optional "Principal" which defaults to the current user
sub HasRight {
    my $self = shift;
    my %args = (
        Right     => undef,
        Principal => $self->CurrentUser,
        @_
    );
    unless ( defined $args{'Principal'} ) {
        $RT::Logger->debug("Principal undefined in Type::HasRight");

    }
    return (
        $args{'Principal'}->HasRight(
            Object => $self->Id ? $self : $RTx::AssetTracker::System,
            Right    => $args{'Right'},
            @_
          )
    );
}

# }}}

# }}}

# {{{ sub Admin

=head2 Admin

Takes nothing.
Returns an RT::Group object which contains this Type's Admins.
If the user doesn't have "ShowType" permission, returns an empty group

This method is here for backwards compatability. All role based methods
are autogenerated based on the AT_SiteConfig file or system defauls.

=cut

sub Admin {
    my $self = shift;

    return $self->AdminRoleGroup(@_);
}

# }}}

# {{{ sub Owner

=head2 Owner

Takes nothing.
Returns an RT::Group object which contains this Type's Owner.
If the user doesn't have "ShowType" permission, returns an empty group

This method is here for backwards compatability. All role based methods
are autogenerated based on the AT_SiteConfig file or system defauls.

=cut

sub Owner {
    my $self = shift;

    return $self->OwnerRoleGroup(@_);
}

# }}}

# {{{ IsWatcher, IsOwner, IsAdmin

# {{{ sub IsWatcher
# a generic routine to be called by IsOwner and IsAdmin

=head2 IsWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID }

Takes a param hash with the attributes Type and PrincipalId

Type is one of Admin and Owner

PrincipalId is an RT::Principal id

Returns true if that principal is a member of the group Type for this asset type


=cut

sub IsWatcher {
    my $self = shift;

    my %args = ( Type  => 'Owner',
        PrincipalId    => undef,
        @_
    );

    # Load the relevant group.
    my $group = $self->LoadTypeRoleGroup(Type => $args{'Type'});
    # Ask if it has the member in question

    my $principal = RT::Principal->new($self->CurrentUser);
    $principal->Load($args{'PrincipalId'});
    unless ($principal->Id) {
        return (undef);
    }

    return ($group->HasMemberRecursively($principal));
}

# }}}


# {{{ sub IsOwner

=head2 IsOwner PRINCIPAL_ID

  Takes an RT::Principal id.
  Returns true if the principal is an owner of the current asset type.

  This method is autogenerated for each AT role. Since they can be
  configured in AT_SiteConfig this method might not make sense in a
  particular installation.

=cut

# }}}

# {{{ sub IsAdmin

=head2 IsAdmin PRINCIPAL_ID

  Takes an RT::Principal id.
  Returns true if the principal is an owner of the current asset type.

  This method is autogenerated for each AT role. Since they can be
  configured in AT_SiteConfig this method might not make sense in a
  particular installation.

=cut

# }}}


# }}}



sub AssetCustomFields {
    my $self = shift;

    my $cfs = RT::CustomFields->new( $self->CurrentUser );
    if ( $self->CurrentUserHasRight('SeeType') ) {
        $cfs->LimitToGlobalOrObjectId( $self->Id );
        $cfs->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
    }
    return ($cfs);
}

# {{{ Routines dealing with watchers.

# {{{ _CreateTypeGroups

=head2 _CreateTypeGroups

Create the asset groups and links for this asset.
This routine expects to be called from Asset->Create _inside of a transaction_

It will create two groups for this asset: Admin and Owner ( Or,
whatever roles were configured in AT_SiteConfig.

It will return true on success and undef on failure.

=begin testing

my $Type = RTx::AssetTracker::Type->new($RT::SystemUser); my ($id, $msg) = $Type->Create(Name => "Foo",);
ok ($id, "Foo $id was created");
ok(my $group = $Type->LoadTypeRoleGroup(Type=> 'Owner'));
ok ($group->Id, "Found the owners object for this Type");

ok(my $bob = RT::User->new($RT::SystemUser), "Creating a bob rt::user");
$bob->LoadOrCreateByEmail('bob@example.com');
ok($bob->Id,  "Found the bob rt user");

ok ((my $add_id, $add_msg) = $Type->AddWatcher(Type => 'Owner', Email => 'bob@example.com'), "Added bob at example.com as a owner");
ok ($add_id, "Add succeeded: ($add_msg)");
ok ($Type->IsWatcher(Type => 'Owner', PrincipalId => $bob->PrincipalId), "The Type actually has bob at example.com as a owner");;
ok ((my $add_id, $add_msg) = $Type->DeleteWatcher(Type =>'Owner', Email => 'bob@example.com'), "Added bob at example.com as a owner");
ok (!$Type->IsWatcher(Type => 'Owner', Principal => $bob->PrincipalId), "The Type no longer has bob at example.com as a owner");;


ok($group = $Type->LoadTypeRoleGroup(Type=> 'Owner'));
ok ($group->Id, "Found the owner object for this Type");
ok($group = $Type->LoadTypeRoleGroup(Type=> 'Admin'));
ok ($group->Id, "Found the Admin object for this Type");

=end testing

=cut


sub _CreateTypeGroups {
    my $self = shift;

    #my @types = qw(Cc Admin Requestor Owner);
    my @types = $self->ActiveRoleArray();

    foreach my $type (@types) {
        my $type_obj = RT::Group->new($self->CurrentUser);
        #my ($id, $msg) = $type_obj->CreateRoleGroup(TypeCheck => 0,
        my ($id, $msg) = $type_obj->_Create(Instance => $self->Id,
                                            Type => $type,
                                            Domain => 'RTx::AssetTracker::Type-Role',
                                            InsideTransaction => 1);
        unless ($id) {
            $RT::Logger->error("Couldn't create a Type group of type '$type' for asset ".
                               $self->Id.": ".$msg);
            return(undef);
        }
     }
    return(1);

}


# }}}

# {{{ sub AddWatcher

=head2 AddWatcher

AddWatcher takes a parameter hash. The keys are as follows:

Type        One of Owner, Admin ( Or any roles names configured in AT_SiteConfig)

PrinicpalId The RT::Principal id of the user or group that's being added as a watcher
Email       The email address of the new watcher. If a user with this
            email address can't be found, a new nonprivileged user will be created.

If the watcher you\'re trying to set has an RT account, set the Owner paremeter to their User Id. Otherwise, set the Email parameter to their Email address.

Returns a tuple of (status/id, message).

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
    if ( $self->CurrentUser->PrincipalId  eq $args{'PrincipalId'}) {
 
        unless ( $self->CurrentUserHasRight('ModifyTypeWatchers')
            or $self->CurrentUserHasRight( $self->RoleRight( $args{'Type'} ) ) ) {
            return ( 0, $self->loc('Permission Denied'));
        }

     else {
            $RT::Logger->warning( "$self -> AddWatcher got passed a bogus type");
            return ( 0, $self->loc('Error in parameters to Type->AddWatcher') );
        }
    }

    # If the watcher isn't the current user
    # and the current user  doesn't have 'ModifyQueueWatcher'
    # bail
    else {
        unless ( $self->CurrentUserHasRight('ModifyTypeWatchers') ) {
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
        @_
    );


    my $principal = RT::Principal->new($self->CurrentUser);
    if ($args{'PrincipalId'}) {
        $principal->Load($args{'PrincipalId'});
    }
    elsif ($args{'Email'}) {

        my $user = RT::User->new($self->CurrentUser);
        $user->LoadByEmail($args{'Email'});

        unless ($user->Id) {
            $user->Load($args{'Email'});
        }
        if ($user->Id) { # If the user exists
            $principal->Load($user->PrincipalId);
        } else {

        # if the user doesn't exist, we need to create a new user
             my $new_user = RT::User->new($RT::SystemUser);

            my ( $Address, $Name ) =
               RT::Interface::Email::ParseAddressFromHeader($args{'Email'});

            my ( $Val, $Message ) = $new_user->Create(
                Name => $Address,
                EmailAddress => $Address,
                RealName     => $Name,
                Privileged   => 0,
                Comments     => 'Autocreated when added as a watcher');
            unless ($Val) {
                $RT::Logger->error("Failed to create user ".$args{'Email'} .": " .$Message);
                # Deal with the race condition of two account creations at once
                $new_user->LoadByEmail($args{'Email'});
            }
            $principal->Load($new_user->PrincipalId);
        }
    }
    # If we can't find this watcher, we need to bail.
    unless ($principal->Id) {
        return(0, $self->loc("Could not find or create that user"));
    }


    my $group = $self->LoadTypeRoleGroup(Type => $args{'Type'});
    unless ($group->id) {
        return(0,$self->loc("Group not found"));
    }

    if ( $group->HasMember( $principal)) {

        return ( 0, $self->loc('That principal is already a [_1] for this type', $args{'Type'}) );
    }


    my ($m_id, $m_msg) = $group->_AddMember(PrincipalId => $principal->Id);
    unless ($m_id) {
        $RT::Logger->error("Failed to add ".$principal->Id." as a member of group ".$group->Id."\n".$m_msg);

        return ( 0, $self->loc('Could not make that principal a [_1] for this type', $args{'Type'}) );
    }
    return ( 1, $self->loc('Added principal as a [_1] for this type', $args{'Type'}) );
}

# }}}

# {{{ sub DeleteWatcher

=head2 DeleteWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID, Email => EMAIL_ADDRESS }


Deletes a type  watcher.  Takes two arguments:

Type  (one of Owner,Admin or any role configured in AT_SiteConfig)

and one of

PrincipalId (an RT::Principal Id of the watcher you want to remove)
    OR
Email (the email address of an existing wathcer)


=cut


sub DeleteWatcher {
    my $self = shift;

    my %args = ( Type => undef,
                 PrincipalId => undef,
                 @_ );

    unless ($args{'PrincipalId'} ) {
        return(0, $self->loc("No principal specified"));
    }
    my $principal = RT::Principal->new($self->CurrentUser);
    $principal->Load($args{'PrincipalId'});

    # If we can't find this watcher, we need to bail.
    unless ($principal->Id) {
        return(0, $self->loc("Could not find that principal"));
    }

    my $group = $self->LoadTypeRoleGroup(Type => $args{'Type'});
    unless ($group->id) {
        return(0,$self->loc("Group not found"));
    }

    # {{{ Check ACLS
    #If the watcher we're trying to add is for the current user
    if ( $self->CurrentUser->PrincipalId  eq $args{'PrincipalId'}) {
        #  If it's an Admin and they don't have
        #   the proper RoleRight or 'ModifyTypeWatchers', bail
        if ( grep { $_ eq $args{'Type'} } $self->ActiveRoleArray() ) {
            unless ( $self->CurrentUserHasRight('ModifyTypeWatchers')
                or $self->CurrentUserHasRight($self->RoleRight( $args{'Type'} )) ) {
                return ( 0, $self->loc('Permission Denied'))
            }
        }

        else {
            $RT::Logger->warning( "$self -> DeleteWatcher got passed a bogus type: $args{'Type'}");
            return ( 0, $self->loc('Error in parameters to Type->DeleteWatcher') );
        }
    }

    # If the watcher isn't the current user
    # and the current user  doesn't have 'ModifyQueueWathcers' bail
    else {
        unless ( $self->CurrentUserHasRight('ModifyTypeWatchers') ) {
            return ( 0, $self->loc("Permission Denied") );
        }
    }

    # }}}


    # see if this user is already a watcher.

    unless ( $group->HasMember($principal)) {
        return ( 0,
        $self->loc('That principal is not a [_1] for this type', $args{'Type'}) );
    }

    my ($m_id, $m_msg) = $group->_DeleteMember($principal->Id);
    unless ($m_id) {
        $RT::Logger->error("Failed to delete ".$principal->Id.
                           " as a member of group ".$group->Id."\n".$m_msg);

        return ( 0,    $self->loc('Could not remove that principal as a [_1] for this type', $args{'Type'}) );
    }

    return ( 1, $self->loc("[_1] is no longer a [_2] for this type.", $principal->Object->Name, $args{'Type'} ));
}

# }}}

# {{{ sub LoadTypeRoleGroup

=head2 LoadTypeRoleGroup  { Type => TYPE }

Loads a Type group from the database.

Takes a param hash with 1 parameters:

    Type is the type of Group we're trying to load:
        Admin, Owner, or any role in AT_SiteConfig

=cut

sub LoadTypeRoleGroup {
    my $self       = shift;
    my %args = (
                Type => undef,
                @_);

    my $group = RT::Group->new( $self->CurrentUser );
    $group->LoadByCols( Domain => 'RTx::AssetTracker::Type-Role',
                       Instance =>$self->Id,
                       Type => $args{'Type'}
                       );

    # if it doesn't exits ( like when we add a new role in the config file )
    # create it
    unless ( $group->id ) {
        my ($id, $msg) = $group->_Create(Instance => $self->Id,
                                            Type => $args{Type},
                                            Domain => 'RTx::AssetTracker::Type-Role',
                                            InsideTransaction => 0);
        unless ($id) {
            $RT::Logger->error("Couldn't create a Type role group of type '$args{Type}' for asset type ".
                               $self->Id.": ".$msg);
        }
    }

    return $group;
}

# }}}

# {{{ RolesForType

=item RolesForType TYPE_ID

Limits the set of groups found to role groups for this type

=cut

sub RolesForType {
    my $self = shift;
    my $type = shift;

    my $groups = RT::Groups->new( $self->CurrentUser );
    $groups->Limit(FIELD => 'Domain', OPERATOR => '=', VALUE => 'RTx::AssetTracker::Type-Role');
    $groups->Limit(FIELD => 'Instance', OPERATOR => '=', VALUE => $self->Id);

    return $groups;
}

# }}}



1;
