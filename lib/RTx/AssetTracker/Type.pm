# BEGIN BPS TAGGED BLOCK {{{
#
# COPYRIGHT:
#
# This software is Copyright (c) 1996-2013 Best Practical Solutions, LLC
#                                          <sales@bestpractical.com>
#
# (Except where explicitly superseded by other copyright notices)
#
#
# LICENSE:
#
# This work is made available to you under the terms of Version 2 of
# the GNU General Public License. A copy of that license should have
# been provided with this software, but in any event can be snarfed
# from www.gnu.org.
#
# This work is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 or visit their web page on the internet at
# http://www.gnu.org/licenses/old-licenses/gpl-2.0.html.
#
#
# CONTRIBUTION SUBMISSION POLICY:
#
# (The following paragraph is not intended to limit the rights granted
# to you to modify and distribute this software under the terms of
# the GNU General Public License and is only of importance to you if
# you choose to contribute your changes and enhancements to the
# community by submitting them to Best Practical Solutions, LLC.)
#
# By intentionally submitting any modifications, corrections or
# derivatives to this work, or any other work intended for use with
# Request Tracker, to Best Practical Solutions, LLC, you confirm that
# you are the copyright holder for those contributions and you grant
# Best Practical Solutions,  LLC a nonexclusive, worldwide, irrevocable,
# royalty-free, perpetual, license to use, copy, create derivative
# works based on those contributions, and sublicense and distribute
# those contributions and any derivatives thereof.
#
# END BPS TAGGED BLOCK }}}

=head1 NAME

  RTx::AssetTracker::Type - an AssetTracker Type object

=head1 SYNOPSIS

  use RTx::AssetTracker::Type;

=head1 DESCRIPTION


=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Type;
use base 'RTx::AssetTracker::Record';

sub Table {'AT_Types'};

use RT::CustomField;
use RT::CustomFields;
use RT::Group;

our $RIGHTS = {
    SeeType            => 'Can this principal see this asset type',       # loc_pair
    AdminType          => 'Create, delete and modify asset types',        # loc_pair
    AssignCustomFields => 'Assign and remove custom fields',              # loc_pair
    ModifyTypeAdmins   => 'Modify administrators for asset type',         # loc_pair
    ModifyTypeWatchers => 'Modify watchers for asset type',               # loc_pair
    ShowAsset      => 'See asset details',                                # loc_pair
    CreateAsset    => 'Create assets of this asset type',                 # loc_pair
    ModifyAsset    => 'Modify assets of this asset type',                 # loc_pair
    RetireAsset    => 'Retire assets of this asset type',                 # loc_pair
    DeleteAsset    => 'Delete assets',                                    # loc_pair
};

our $RIGHT_CATEGORIES = {
    SeeType            => 'General',
    ShowAsset          => 'General',
    ModifyAsset        => 'General',
    CreateAsset        => 'General',
    AdminType          => 'Admin',
    AssignCustomFields => 'Staff',
    ModifyTypeAdmins   => 'Staff',
    ModifyTypeWatchers => 'Staff',
    RetireAsset        => 'Staff',
    DeleteAsset        => 'Staff',
};

# Tell RT::ACE that this sort of object can get acls granted
$RT::ACE::OBJECT_TYPES{'RTx::AssetTracker::Type'} = 1;

# TODO: This should be refactored out into an RT::ACLedObject or something
# stuff the rights into a hash of rights that can exist.

__PACKAGE__->AddRights(%$RIGHTS);
__PACKAGE__->AddRightCategories(%$RIGHT_CATEGORIES);

RT::System::AddRights(%$RIGHTS);
RT::System::AddRightCategories(%$RIGHT_CATEGORIES);

require RT::Lifecycle;

=head2 AddRights C<RIGHT>, C<DESCRIPTION> [, ...]

Adds the given rights to the list of possible rights.  This method
should be called during server startup, not at runtime.

=cut

sub AddRights {
    my $self = shift;
    my %new = @_;
    $RIGHTS = { %$RIGHTS, %new };
    %RT::ACE::LOWERCASERIGHTNAMES = ( %RT::ACE::LOWERCASERIGHTNAMES,
                                      map { lc($_) => $_ } keys %new);
}

=head2 AddRightCategories C<RIGHT>, C<CATEGORY> [, ...]

Adds the given right and category pairs to the list of right categories.  This
method should be called during server startup, not at runtime.

=cut

sub AddRightCategories {
    my $self = shift if ref $_[0] or $_[0] eq __PACKAGE__;
    my %new = @_;
    $RIGHT_CATEGORIES = { %$RIGHT_CATEGORIES, %new };
}


# Custom field support
RT::CustomField->_ForObjectType( 'RTx::AssetTracker::Type' => "Asset Types" );

# {{{ Setup Roles/Watchers

our %DEFAULT_ROLES = (
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
    my $self = shift;

    foreach my $role ( $self->RoleGroupTypes ) {
        $self->ConfigureRole( $role );
    }

}

sub ConfigureRole {

    my $self = shift;
    my $role = shift;

    # if the system role group doesn't exist, create it
    my $group = RT::Group->new( $RT::SystemUser );
    $group->LoadByCols( Domain => 'RT::System-Role', Type => $role );
    unless ( $group->id ) {
        $group->_Create( Domain            => 'RT::System-Role',
                         Instance          => 0,
                         Type              => $role,
                         Description       => 'SystemRolegroup for internal use',  # loc
                         InsideTransaction => 0 );
        $group->id or $RT::Logger->error("Couldn't create group for system role '$role'");
    }

    $RTx::AssetTracker::Assets::FIELD_METADATA{$role}         = [ 'WATCHERFIELD'    => $role ];
    $RTx::AssetTracker::Assets::FIELD_METADATA{'Type'.$role}  = [ 'WATCHERFIELD'    => $role => 'Type' ];
    $RTx::AssetTracker::Assets::FIELD_METADATA{$role.'Group'} = [ 'MEMBERSHIPFIELD' => $role ];
    $RTx::AssetTracker::Assets::LOWER_CASE_FIELDS{lc $role}         = $role;
    $RTx::AssetTracker::Assets::LOWER_CASE_FIELDS{lc 'Type'.$role}  = 'Type'.$role;
    $RTx::AssetTracker::Assets::LOWER_CASE_FIELDS{lc $role.'Group'} = $role.'Group';

    my $right = $self->RoleRight($role);
    my $desc = $self->RoleDescription($role);

    $self->AddRights( $right => $desc );
    $self->AddRightCategories( $right => 'General' );
    RT::System->AddRights( $right => $desc );
    RT::System->AddRightCategories( $right => 'General' );

    $RT::ACE::LOWERCASERIGHTNAMES{ lc $right } = $right;

    my $group_role_method = $role . "RoleGroup";
    my $is_role_method = 'Is' . $role;
    my $export_role_method = $role . "RoleGroupExportString";

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

        *$export_role_method = sub {
            my $self  = shift;

            my $export_string = undef;
            if ($group = $self->LoadAssetRoleGroup(Type => $role)) {
                my $members = $group->MembersObj->ItemsArrayRef;
                $export_string = join(",", map { $_->MemberObj->IsGroup ? '@'. $_->MemberObj->Object->Name()
                                                                        : $_->MemberObj->Object->EmailAddress } @$members);
            }
            return $export_string;

        };

    }


}


# }}}


=head2 AvailableRights

Returns a hash of available rights for this object. The keys are the right names and the values are a description of what the rights do

=cut

sub AvailableRights {
    my $self = shift;
    return($RIGHTS);
}

=head2 RightCategories

Returns a hashref where the keys are rights for this type of object and the
values are the category (General, Staff, Admin) the right falls into.

=cut

sub RightCategories {
    return $RIGHT_CATEGORIES;
}


sub Lifecycle {
    my $self = shift;
    unless (ref $self && $self->id) { 
        return RT::Lifecycle->Load('')
    }

    my $name = $self->_Value( Lifecycle => @_ );
    $name ||= 'at_default';

    my $res = RT::Lifecycle->Load( $name );
    unless ( $res ) {
        $RT::Logger->error("Lifecycle '$name' for asset type '".$self->Name."' doesn't exist");
        return RT::Lifecycle->Load('at_default');
    }
    return $res;
}

sub SetLifecycle {
    my $self = shift;
    my $value = shift || 'at_default';

    return ( 0, $self->loc( '[_1] is not a valid lifecycle', $value ) )
      unless $self->ValidateLifecycle($value);

    return $self->_Set( Field => 'Lifecycle', Value => $value, @_ );
}

=head2 ValidateLifecycle NAME

Takes a lifecycle name. Returns true if it's an ok name and such
lifecycle is configured. Returns undef otherwise.

=cut

sub ValidateLifecycle {
    my $self = shift;
    my $value = shift;
    return undef unless RT::Lifecycle->Load( $value );
    return 1;
}


=head2 ActiveStatusArray

Returns an array of all ActiveStatuses for this asset type

=cut

sub ActiveStatusArray {
    my $self = shift;
    return $self->Lifecycle->Valid('initial', 'active');
}

=head2 InactiveStatusArray

Returns an array of all InactiveStatuses for this asset type

=cut

sub InactiveStatusArray {
    my $self = shift;
    return $self->Lifecycle->Inactive;
}

=head2 StatusArray

Returns an array of all statuses for this asset type

=cut

sub StatusArray {
    my $self = shift;
    return $self->Lifecycle->Valid( @_ );
}

=head2 IsValidStatus value

Returns true if value is a valid status.  Otherwise, returns 0.

=cut

sub IsValidStatus {
    my $self  = shift;
    return $self->Lifecycle->IsValid( shift );
}

=head2 IsActiveStatus value

Returns true if value is a Active status.  Otherwise, returns 0

=cut

sub IsActiveStatus {
    my $self  = shift;
    return $self->Lifecycle->IsValid( shift, 'initial', 'active');
}



=head2 IsInactiveStatus value

Returns true if value is a Inactive status.  Otherwise, returns 0


=cut

sub IsInactiveStatus {
    my $self  = shift;
    return $self->Lifecycle->IsInactive( shift );
}






=head2 Create

Create takes the name of the new type
If you pass the ACL check, it creates the type and returns its type id.

=cut

sub Create {
    my $self = shift;
    my %args = (
        Name              => undef,
        Description       => '',
        Lifecycle         => 'at_default',
        _RecordTransaction => 1,
        @_
    );

    unless ( $self->CurrentUser->HasRight(Right => 'AdminType', Object => $RT::System) )
    {    #Check them ACLs
        return ( 0, $self->loc("No permission to create asset types") );
    }

    {
        my ($val, $msg) = $self->_ValidateName( $args{'Name'} );
        return ($val, $msg) unless $val;
    }

    $args{'Lifecycle'} ||= 'at_default';

    return ( 0, $self->loc('[_1] is not a valid lifecycle', $args{'Lifecycle'} ) )
      unless $self->ValidateLifecycle( $args{'Lifecycle'} );

    my %attrs = map {$_ => 1} $self->ReadableAttributes;

    #TODO better input validation
    $RT::Handle->BeginTransaction();
    my $id = $self->SUPER::Create( map { $_ => $args{$_} } grep exists $args{$_}, keys %attrs );
    unless ($id) {
        $RT::Handle->Rollback();
        return ( 0, $self->loc('Asset type could not be created') );
    }

    my $create_ret = $self->_CreateTypeGroups();
    unless ($create_ret) {
        $RT::Handle->Rollback();
        return ( 0, $self->loc('Asset type could not be created') );
    }
    if ( $args{'_RecordTransaction'} ) {
        $self->_NewTransaction( Type => "Create" );
    }
    $RT::Handle->Commit;

    #RT->System->AssetTypeCacheNeedsUpdate(1);

    return ( $id, $self->loc("Asset type created") );
}



sub Delete {
    my $self = shift;
    return ( 0,
        $self->loc('Deleting this object would break referential integrity') );
}



=head2 SetDisabled

Takes a boolean.
1 will cause this asset type to no longer be available for assets.
0 will re-enable this asset type.

=cut

sub SetDisabled {
    my $self = shift;
    my $val = shift;

    $RT::Handle->BeginTransaction();
    my $set_err = $self->_Set( Field =>'Disabled', Value => $val);
    unless ($set_err) {
        $RT::Handle->Rollback();
        $RT::Logger->warning("Couldn't ".($val == 1) ? "disable" : "enable"." asset type ".$self->PrincipalObj->Id);
        return (undef);
    }
    $self->_NewTransaction( Type => ($val == 1) ? "Disabled" : "Enabled" );

    $RT::Handle->Commit();

    #RT->System->AssetTypeCacheNeedsUpdate(1);

    if ( $val == 1 ) {
        return (1, $self->loc("Asset Type disabled"));
    } else {
        return (1, $self->loc("Asset Type enabled"));
    }

}



=head2 Load

Takes either a numerical id or a textual Name and loads the specified asset type.

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



=head2 ValidateName NAME

Takes an asset type name. Returns true if it's an ok name for
a new asset type. Returns undef if there's already an asset type by that name.

=cut

sub ValidateName {
    my $self = shift;
    my $name = shift;

    my ($ok, $msg) = $self->_ValidateName($name);

    return $ok ? 1 : 0;
}

sub _ValidateName {
    my $self = shift;
    my $name = shift;

    return (undef, "Asset type name is required") unless length $name;

    # Validate via the superclass first
    # Case: short circuit if it's an integer so we don't have
    # fale negatives when loading a temp asset type
    unless ( my $q = $self->SUPER::ValidateName($name) ) {
        return ($q, $self->loc("'[_1]' is not a valid name.", $name));
    }

    my $temptype = RTx::AssetTracker::Type->new(RT->SystemUser);
    $temptype->Load($name);

    #If this asset type exists, return undef
    if ( $temptype->Name() && $temptype->id != $self->id)  {
        return (undef, $self->loc("Asset type already exists") );
    }

    return (1);
}


=head2 Templates

Returns an RTx::AssetTracker::Templates object of all of this asset type's templates.

=cut

sub Templates {
    my $self = shift;

    my $templates = RTx::AssetTracker::Templates->new( $self->CurrentUser );

    if ( $self->CurrentUserHasRight('ShowTemplate') ) {
        $templates->LimitToAssetType( $self->id );
    }

    return ($templates);
}




=head2 CustomField NAME

Load the asset type-specific custom field named NAME

=cut

sub CustomField {
    my $self = shift;
    my $name = shift;
    my $cf = RT::CustomField->new($self->CurrentUser);
    $cf->LoadByNameAndAssetType(Name => $name, Type => $self->Id);
    return ($cf);
}



=head2 AssetCustomFields

Returns an L<RT::CustomFields> object containing all global and
type-specific B<asset> custom fields.

=cut

sub AssetCustomFields {
    my $self = shift;

    my $cfs = RT::CustomFields->new( $self->CurrentUser );
    if ( $self->CurrentUserHasRight('SeeType') ) {
        $cfs->SetContextObject( $self );
        $cfs->LimitToGlobalOrObjectId( $self->Id );
        $cfs->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' );
        $cfs->ApplySortOrder;
    }
    return ($cfs);
}



=head2 AssetTransactionCustomFields

Returns an L<RT::CustomFields> object containing all global and
type-specific B<transaction> custom fields.

=cut

sub AssetTransactionCustomFields {
    my $self = shift;

    my $cfs = RT::CustomFields->new( $self->CurrentUser );
    if ( $self->CurrentUserHasRight('SeeType') ) {
        $cfs->SetContextObject( $self );
	$cfs->LimitToGlobalOrObjectId( $self->Id );
	$cfs->LimitToLookupType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset-RT::Transaction' );
        $cfs->ApplySortOrder;
    }
    return ($cfs);
}





=head2 RoleGroupTypes

Returns a list of the names of the various role group types that this
asset type has.

=cut

sub RoleGroupTypes {
    my $self = shift;

    if (@RT::AssetRoles) {
        return (@RT::AssetRoles)
    } else {
        return (sort keys %DEFAULT_ROLES);
    }
}

=head2 IsRoleGroupType

Returns whether the passed-in type is a role group type.

=cut

sub IsRoleGroupType {
    my $self = shift;
    my $type = shift;

    for my $valid_type ($self->RoleGroupTypes) {
        return 1 if $type eq $valid_type;
    }

    return 0;
}


=head2 _CreateTypeGroups

Create the asset groups and links for this asset.
This routine expects to be called from Asset->Create _inside of a transaction_

It will create two groups for this asset: Admin and Owner ( Or,
whatever roles were configured in AT_SiteConfig.

It will return true on success and undef on failure.

=cut

sub _CreateTypeGroups {
    my $self = shift;

    my @types = $self->RoleGroupTypes();

    foreach my $type (@types) {
        my $ok = $self->_CreateTypeRoleGroup($type);
        return undef if !$ok;
    }

    return 1;
}


# Wrap RT::Group::CreateRoleGroup so that it can deal with groups for
# the RTx::AssetTracker domain.
{
    package RT::Group;
    no warnings qw(redefine);

    my $Orig_CreateRoleGroup = __PACKAGE__->can('CreateRoleGroup')
        or die "API change? Can't find method 'CreateRoleGroup'";
    *CreateRoleGroup = sub {
        my $self = shift;
        my %args = ( Instance => undef,
                     Type     => undef,
                     Domain   => undef,
                     @_ );

        return $Orig_CreateRoleGroup->($self, %args)
            unless $args{'Domain'} =~ /^RTx::AssetTracker/;

        unless (RTx::AssetTracker::Type->IsRoleGroupType($args{Type})) {
            return ( 0, $self->loc("Invalid Group Type") );
        }


        return ( $self->_Create( Domain            => $args{'Domain'},
                                 Instance          => $args{'Instance'},
                                 Type              => $args{'Type'},
                                 InsideTransaction => 1 ) );
    }
}


sub _CreateTypeRoleGroup {
    my $self = shift;
    my $type = shift;

    my $type_obj = RT::Group->new($self->CurrentUser);
    my ($id, $msg) = $type_obj->CreateRoleGroup(Instance => $self->Id, 
                                                    Type => $type,
                                                    Domain => 'RTx::AssetTracker::Type-Role');
    unless ($id) {
        $RT::Logger->error("Couldn't create an Asset Type group of type '$type' for type ".
                            $self->Id.": ".$msg);
        return(undef);
    }

    return $id;
}



# _HasModifyWatcherRight {{{
sub _HasModifyWatcherRight {
    my $self = shift;
    my %args = (
        Type  => undef,
        PrincipalId => undef,
        Email => undef,
        @_
    );

    return 1 if $self->CurrentUserHasRight('ModifyTypeWatchers');

    #If the watcher we're trying to add is for the current user
    if ( defined $args{'PrincipalId'} && $self->CurrentUser->PrincipalId  eq $args{'PrincipalId'}) {

        if ( $self->IsRoleGroupType( $args{'Type'} ) ) {
            return 1 if $self->CurrentUserHasRight( $self->RoleRight($args{'Type'}) );
        }
        else {
            $RT::Logger->warning( "$self -> _HasModifyWatcher got passed a bogus type $args{Type}");
            return ( 0, $self->loc('Invalid asset type role group type [_1]', $args{Type}) );
        }
    }

    return ( 0, $self->loc("Permission Denied") );
}


=head2 AddWatcher

AddWatcher takes a parameter hash. The keys are as follows:

Type        One of Owner, Admin ( Or any roles names configured in AT_SiteConfig)

PrinicpalId The RT::Principal id of the user or group that's being added as a watcher
Email       The email address of the new watcher. If a user with this
            email address can't be found, a new nonprivileged user will be created.

If the watcher you're trying to set has an RT account, set the Owner paremeter to their User Id. Otherwise, set the Email parameter to their Email address.

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

    return ( 0, "No principal specified" )
        unless $args{'Email'} or $args{'PrincipalId'};

    if ( !$args{'PrincipalId'} && $args{'Email'} ) {
        my $user = RT::User->new( $self->CurrentUser );
        $user->LoadByEmail( $args{'Email'} );
        $args{'PrincipalId'} = $user->PrincipalId if $user->id;
    }

    return ( 0, "Unknown watcher type [_1]", $args{Type} )
        unless $self->IsRoleGroupType($args{Type});

    my ($ok, $msg) = $self->_HasModifyWatcherRight(%args);
    return ($ok, $msg) if !$ok;

    return $self->_AddWatcher(%args);
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


    my $principal = RT::Principal->new( $self->CurrentUser );
    if ( $args{'PrincipalId'} ) {
        $principal->Load( $args{'PrincipalId'} );
        if ( $principal->id and $principal->IsUser and my $email = $principal->Object->EmailAddress ) {
            return (0, $self->loc("[_1] is an address RT receives mail at. Adding it as a '[_2]' would create a mail loop", $email, $self->loc($args{'Type'})))
                if RT::EmailParser->IsRTAddress( $email );
        }
    }
    elsif ( $args{'Email'} ) {
        if ( RT::EmailParser->IsRTAddress( $args{'Email'} ) ) {
            return (0, $self->loc("[_1] is an address RT receives mail at. Adding it as a '[_2]' would create a mail loop", $args{'Email'}, $self->loc($args{'Type'})));
        }
        my $user = RT::User->new($self->CurrentUser);
        $user->LoadByEmail( $args{'Email'} );
        $user->Load( $args{'Email'} )
            unless $user->id;

        if ( $user->Id ) { # If the user exists
            $principal->Load( $user->PrincipalId );
        } else {
            # if the user doesn't exist, we need to create a new user
            my $new_user = RT::User->new(RT->SystemUser);

            my ( $Address, $Name ) =  
               RT::Interface::Email::ParseAddressFromHeader($args{'Email'});

            my ( $Val, $Message ) = $new_user->Create(
                Name         => $Address,
                EmailAddress => $Address,
                RealName     => $Name,
                Privileged   => 0,
                Comments     => 'Autocreated when added as a watcher'
            );
            unless ($Val) {
                $RT::Logger->error("Failed to create user ".$args{'Email'} .": " .$Message);
                # Deal with the race condition of two account creations at once
                $new_user->LoadByEmail( $args{'Email'} );
            }
            $principal->Load( $new_user->PrincipalId );
        }
    }
    # If we can't find this watcher, we need to bail.
    unless ( $principal->Id ) {
        return(0, $self->loc("Could not find or create that user"));
    }

    my $group = $self->LoadTypeRoleGroup(Type => $args{'Type'});
    unless ($group->id) {
        return(0,$self->loc("Group not found"));
    }

    if ( $group->HasMember( $principal)) {

        return ( 0, $self->loc('[_1] is already a [_2] for this asset type',
                    $principal->Object->Name, $args{'Type'}) );
    }


    my ($m_id, $m_msg) = $group->_AddMember(PrincipalId => $principal->Id);
    unless ($m_id) {
        $RT::Logger->error("Failed to add ".$principal->Id." as a member of group ".$group->Id.": ".$m_msg);

        return ( 0, $self->loc('Could not make [_1] a [_2] for this asset type',
                    $principal->Object->Name, $args{'Type'}) );
    }
    return ( 1, $self->loc("Added [_1] to members of [_2] for this asset type.", $principal->Object->Name, $args{'Type'} ));
}



=head2 DeleteWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID, Email => EMAIL_ADDRESS }


Deletes an asset type watcher.  Takes two arguments:

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
                 Email => undef,
                 @_ );

    unless ( $args{'PrincipalId'} || $args{'Email'} ) {
        return ( 0, $self->loc("No principal specified") );
    }

    if ( !$args{PrincipalId} and $args{Email} ) {
        my $user = RT::User->new( $self->CurrentUser );
        my ($rv, $msg) = $user->LoadByEmail( $args{Email} );
        $args{PrincipalId} = $user->PrincipalId if $rv;
    }
    
    my $principal = RT::Principal->new( $self->CurrentUser );
    if ( $args{'PrincipalId'} ) {
        $principal->Load( $args{'PrincipalId'} );
    }
    else {
        my $user = RT::User->new( $self->CurrentUser );
        $user->LoadByEmail( $args{'Email'} );
        $principal->Load( $user->Id );
    }

    # If we can't find this watcher, we need to bail.
    unless ( $principal->Id ) {
        return ( 0, $self->loc("Could not find that principal") );
    }

    my $group = $self->LoadTypeRoleGroup(Type => $args{'Type'});
    unless ($group->id) {
        return(0,$self->loc("Group not found"));
    }

    return ( 0, $self->loc('Unknown watcher type [_1]', $args{Type}) )
        unless $self->IsRoleGroupType($args{Type});

    my ($ok, $msg) = $self->_HasModifyWatcherRight(%args);
    return ($ok, $msg) if !$ok;

    # see if this user is already a watcher.

    unless ( $group->HasMember($principal)) {
        return ( 0, $self->loc('[_1] is not a [_2] for this asset type',
            $principal->Object->Name, $args{'Type'}) );
    }

    my ($m_id, $m_msg) = $group->_DeleteMember($principal->Id);
    unless ($m_id) {
        $RT::Logger->error("Failed to delete ".$principal->Id.
                           " as a member of group ".$group->Id.": ".$m_msg);

        return ( 0, $self->loc('Could not remove [_1] as a [_2] for this asset type',
                    $principal->Object->Name, $args{'Type'}) );
    }

    return ( 1, $self->loc("Removed [_1] from members of [_2] for this asset type.", $principal->Object->Name, $args{'Type'} ));
}


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




=head2 IsOwner PRINCIPAL_ID

Takes an RT::Principal id.
Returns true if the principal is an owner of the current asset type.

  This method is autogenerated for each AT role. Since they can be
  configured in AT_SiteConfig this method might not make sense in a
  particular installation.

=cut



=head2 IsAdmin PRINCIPAL_ID

Takes an RT::Principal id.
Returns true if the principal is an owner of the current asset type.

  This method is autogenerated for each AT role. Since they can be
  configured in AT_SiteConfig this method might not make sense in a
  particular installation.

=cut




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



### Shredder methods ###

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

# Tickets
    my $objs = RTx::AssetTracker::Assets->new( $self->CurrentUser );
    $objs->{'allow_deleted_search'} = 1;
    $objs->Limit( FIELD => 'Type', VALUE => $self->Id );
    push( @$list, $objs );

# Type role groups( Owner, Admin )
    $objs = RT::Groups->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Domain', VALUE => 'RTx::AssetTracker::Type-Role' );
    $objs->Limit( FIELD => 'Instance', VALUE => $self->Id );
    push( @$list, $objs );
# Custom Fields
    $objs = RT::CustomFields->new( $self->CurrentUser );
    $objs->LimitToAssetType( $self->id );
    push( @$list, $objs );

    $deps->_PushDependencies(
            BaseObject => $self,
            Flags => DEPENDS_ON,
            TargetObjects => $list,
            Shredder => $args{'Shredder'}
        );
    return $self->SUPER::__DependsOn( %args );
}




sub _Set {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('AdminType') ) {
        return ( 0, $self->loc('Permission Denied') );
    }
    #RT->System->AssetTypeCacheNeedsUpdate(1);
    return ( $self->SUPER::_Set(@_) );
}



sub _Value {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('SeeType') ) {
        return (undef);
    }

    return ( $self->__Value(@_) );
}



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
            Right     => "$right"
          )
    );

}

=head2 CurrentUserCanSee

Returns true if the current user can see the type, using SeeType

=cut

sub CurrentUserCanSee {
    my $self = shift;

    return $self->CurrentUserHasRight('SeeType');
}


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
    my $principal = delete $args{'Principal'};
    unless ( $principal ) {
        $RT::Logger->error("Principal undefined in Type::HasRight");
        return undef;
    }

    return $principal->HasRight(
        %args,
        Object => ($self->Id ? $self : $RT::System),
    );
}




=head2 id

Returns the current value of id. 
(In the database, id is stored as int(11).)


=cut


=head2 Name

Returns the current value of Name. 
(In the database, Name is stored as varchar(200).)



=head2 SetName VALUE


Set Name to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Name will be stored as a varchar(200).)


=cut


=head2 Description

Returns the current value of Description. 
(In the database, Description is stored as varchar(255).)



=head2 SetDescription VALUE


Set Description to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Description will be stored as a varchar(255).)


=cut


=head2 DefaultAdmin

Returns the current value of DefaultAdmin. 
(In the database, DefaultAdmin is stored as int(11).)



=head2 SetDefaultAdmin VALUE


Set DefaultAdmin to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, DefaultAdmin will be stored as a int(11).)


=cut


=head2 Lifecycle

Returns the current value of Lifecycle. 
(In the database, Lifecycle is stored as varchar(32).)



=head2 SetLifecycle VALUE


Set Lifecycle to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Lifecycle will be stored as a varchar(32).)


=cut


=head2 Creator

Returns the current value of Creator. 
(In the database, Creator is stored as int(11).)


=cut


=head2 Created

Returns the current value of Created. 
(In the database, Created is stored as datetime.)


=cut


=head2 LastUpdatedBy

Returns the current value of LastUpdatedBy. 
(In the database, LastUpdatedBy is stored as int(11).)


=cut


=head2 LastUpdated

Returns the current value of LastUpdated. 
(In the database, LastUpdated is stored as datetime.)


=cut


=head2 Disabled

Returns the current value of Disabled. 
(In the database, Disabled is stored as smallint(6).)



=head2 SetDisabled VALUE


Set Disabled to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Disabled will be stored as a smallint(6).)


=cut



sub _CoreAccessible {
    {
     
        id =>
        {read => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => ''},
        Name => 
        {read => 1, write => 1, sql_type => 12, length => 200,  is_blob => 0,  is_numeric => 0,  type => 'varchar(200)', default => ''},
        Description => 
        {read => 1, write => 1, sql_type => 12, length => 255,  is_blob => 0,  is_numeric => 0,  type => 'varchar(255)', default => ''},
        DefaultAdmin => 
        {read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Lifecycle => 
        {read => 1, write => 1, sql_type => 12, length => 32,  is_blob => 0, is_numeric => 0,  type => 'varchar(32)', default => 'at_default'},
        Creator => 
        {read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Created => 
        {read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        LastUpdatedBy => 
        {read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        LastUpdated => 
        {read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        Disabled => 
        {read => 1, write => 1, sql_type => 5, length => 6,  is_blob => 0,  is_numeric => 1,  type => 'smallint(6)', default => '0'},

 }
};

sub Dependencies {
    my $self = shift;
    my ($walker, $deps) = @_;

    $self->SUPER::Dependencies($walker, $deps);

    # role groups( Owner, Admin )
    my $objs = RT::Groups->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Domain', VALUE => 'RTx::AssetTracker::Type-Role' );
    $objs->Limit( FIELD => 'Instance', VALUE => $self->Id );
    $deps->Add( in => $objs );

    # Scrips
    $objs = RTx::AssetTracker::Scrips->new( $self->CurrentUser );
    $objs->LimitToAssetType( $self->id );
    $deps->Add( in => $objs );

    # Templates (global ones have already been dealt with)
    $objs = RTx::AssetTracker::Templates->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'AssetType', VALUE => $self->Id);
    $deps->Add( in => $objs );

    # Custom Fields on assets of this type (CFs on the type itself
    # have already been dealt with)
    $objs = RT::ObjectCustomFields->new( $self->CurrentUser );
    $objs->Limit( FIELD           => 'ObjectId',
                  OPERATOR        => '=',
                  VALUE           => $self->id,
                  ENTRYAGGREGATOR => 'OR' );
    $objs->Limit( FIELD           => 'ObjectId',
                  OPERATOR        => '=',
                  VALUE           => 0,
                  ENTRYAGGREGATOR => 'OR' );
    my $cfs = $objs->Join(
        ALIAS1 => 'main',
        FIELD1 => 'CustomField',
        TABLE2 => 'CustomFields',
        FIELD2 => 'id',
    );
    $objs->Limit( ALIAS    => $cfs,
                  FIELD    => 'LookupType',
                  OPERATOR => 'STARTSWITH',
                  VALUE    => 'RTx::AssetTracker::Type-' );
    $deps->Add( in => $objs );

    # Assets
    $objs = RTx::AssetTracker::Assets->new( $self->CurrentUser );
    $objs->Limit( FIELD => 'Type', VALUE => $self->Id );
    $objs->{allow_deleted_search} = 1;
    $deps->Add( in => $objs );
}



RT::Base->_ImportOverlays();

1;
