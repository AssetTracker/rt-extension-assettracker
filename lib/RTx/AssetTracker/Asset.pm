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

  RTx::AssetTracker::Asset - an AssetTracker Asset object

=head1 SYNOPSIS

  use RTx::AssetTracker::Asset;

=head1 DESCRIPTION


=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Asset;
use base 'RTx::AssetTracker::Record';

sub Table {'AT_Assets'};

use RTx::AssetTracker::Type;
use RTx::AssetTracker::Assets;
use RT::Group;
use RT::URI::at;
use RTx::AssetTracker::IPs;
use RTx::AssetTracker::Ports;
use RT::URI;
use RT::CustomField;

RT::CustomField->_ForObjectType( 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset' => "Assets" );


# A helper table for links mapping to make it easier
# to build and parse links between assets

our %LINKMAP     = ();
our @LINKORDER   = ();
our %LINKTYPEMAP = ();
our %LINKDIRMAP  = ();


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
    } unless $class->can($base);

    *$target = sub {
        my $self = shift;
        return ( $self->_Links( $LINKTYPEMAP{$base}{Mode},   $base ) );
    } unless $class->can($target);

    }

    # sets up the Limit methods for links
    #RTx::AssetTracker::Assets::RegisterLinkField($base, $target);
    $RTx::AssetTracker::Assets::FIELD_METADATA{$base}   = [ 'LINK' => To   => $base ];
    $RTx::AssetTracker::Assets::FIELD_METADATA{$target} = [ 'LINK' => From => $base ];
    $RTx::AssetTracker::Assets::LOWER_CASE_FIELDS{lc $base}   = $base;
    $RTx::AssetTracker::Assets::LOWER_CASE_FIELDS{lc $target} = $target;

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
        } unless $class->can($limit_base);

        *$limit_target = sub {
            my $self = shift;
            my $asset_uri = shift;
            $self->LimitLinkedFrom ( BASE => $asset_uri,
                                     TYPE => $target,        );
        } unless $class->can($limit_target);

    }


}


sub LINKMAP       { return \%LINKMAP   }
sub LINKTYPEMAP   { return \%LINKTYPEMAP   }
sub LINKDIRMAP    { return \%LINKDIRMAP   }
sub LINKORDER     { return  @LINKORDER   }

sub ConfigureLinks {
    my $class = shift;

    my $map = RT->Config->Get('AssetLinkTypes') or return;

    while ( my ($forward, $reverse) = each %$map ) {
        $class->RegisterLinkType( $forward, $reverse );
    }
}


=head2 Load

Takes a single argument. This can be an asset id or
asset name.  If the asset can't be loaded, returns undef.
Otherwise, returns the asset id.

=cut

sub Load {
    my $self = shift;
    my $id   = shift;
    $id = '' unless defined $id;

    #If we have an integer URI, load the asset
    if ( $id =~ /^\d+$/ ) {
        my ($assetid,$msg) = $self->LoadById($id);

        unless ($self->Id) {
            $RT::Logger->debug("$self tried to load a bogus asset: $id");
            return (undef);
        }
    }

    elsif ( $id ) {
        my ($assetid,$msg) = $self->LoadByCol('Name', $id);

        unless ($self->Id) {
            $RT::Logger->debug("$self tried to load a bogus asset named: $id");
            return (undef);
        }
    }

    else {
        $RT::Logger->debug("Tried to load a bogus asset id: '$id'");
        return (undef);
    }

    #Ok. we're loaded. lets get outa here.
    return $self->Id;
}



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

=cut

sub Create {
    my $self = shift;

    my %args = (
        id                 => undef,
        Type               => undef,
        Name               => undef,
        Description        => '',
        Status             => undef,
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

    my $TypeObj = RTx::AssetTracker::Type->new( RT->SystemUser );
    if ( ref $args{'Type'} eq 'RTx::AssetTracker::Type' ) {
        $TypeObj->Load( $args{'Type'}->Id );
    }
    elsif ( $args{'Type'} ) {
        $TypeObj->Load( $args{'Type'} );
    }
    else {
        $RT::Logger->debug("'". ( $args{'Type'} ||'') . "' not a recognized type object." );
    }

    #Can't create an asset without a type.
    unless ( $TypeObj->Id ) {
        $RT::Logger->debug("$self No type given for asset creation.");
        return ( 0, 0, $self->loc('Could not create asset. Type not set') );
    }


    #Now that we have a type, Check the ACLS
    unless (
        $self->CurrentUser->HasRight(
            Right  => 'CreateAsset',
            Object => $TypeObj,
        )
      )
    {
        return (
            0, 0,
            $self->loc( "No permission to create assets of type '[_1]'", $TypeObj->Name));
    }

    my $cycle = $TypeObj->Lifecycle;
    unless ( defined $args{'Status'} && length $args{'Status'} ) {
        $args{'Status'} = $cycle->DefaultOnCreate;
    }

    $args{'Status'} = lc $args{'Status'};
    unless ( $cycle->IsValid( $args{'Status'} ) ) {
        return ( 0, 0,
            $self->loc("Status '[_1]' isn't a valid status for assets of this type.",
                $self->loc($args{'Status'}))
        );
    }

    unless ( $cycle->IsTransition( '' => $args{'Status'} ) ) {
        return ( 0, 0,
            $self->loc("New assets of this type can not have status '[_1]'.",
                $self->loc($args{'Status'}))
        );
    }

    $args{'Name'} =~ s/\n//g;
    $args{'Description'} =~ s/\n//g;

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
    foreach my $attr (qw(id Creator Created LastUpdated LastUpdatedBy)) {
        $params{$attr} = $args{$attr} if $args{$attr};
    }

    my ($id,$asset_message) = $self->SUPER::Create( %params );
    unless ($id) {
        $RT::Logger->crit( "Couldn't create an asset: " . $asset_message );
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


    # Deal with setting up watchers

    foreach my $type ( RTx::AssetTracker::Type->RoleGroupTypes() ) {
        next unless ( defined $args{$type} );
        foreach my $watcher (
            ref( $args{$type} ) ? @{ $args{$type} } : ( $args{$type} ) )
        {

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

            # Note that we're using AddWatcher, rather than _AddWatcher, as we
            # actually _want_ that ACL check. Otherwise, random asset creators
            # could make themselves admins and maybe get asset rights. that would
            # be poor
            my ( $val, $msg ) = $self->AddWatcher(
                Type   => $type,
                $field => $watcher,
                Silent => 1,
            );
            push @non_fatal_errors, $self->loc("Couldn't set [_1] watcher: [_2]", $type, $msg)
                unless $val;
        }
    }


    # }}}

    # Add all the custom fields

    foreach my $arg ( keys %args ) {
        next unless $arg =~ /^CustomField-(\d+)$/i;
        my $cfid = $1;

        foreach my $value (
            UNIVERSAL::isa( $args{$arg} => 'ARRAY' ) ? @{ $args{$arg} } : ( $args{$arg} ) )
        {
            next unless defined $value && length $value;

            # Allow passing in uploaded LargeContent etc by hash reference
            my ($status, $msg) = $self->_AddCustomFieldValue(
                (UNIVERSAL::isa( $value => 'HASH' )
                    ? %$value
                    : (Value => $value)
                ),
                Field             => $cfid,
                RecordTransaction => 0,
            );
            push @non_fatal_errors, $msg unless $status;
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
            Type         => "Create",
            Data         => $args{TransactionData},
            CommitScrips => !$args{'DryRun'},
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
            my ( $val, $msg, $obj ) = $self->__GetAssetFromURI( URI => $link );
            unless ($val) {
                push @errors, $msg;
                next;
            }

            # Check rights on the other end of the link if we must
            # then run _AddLink that doesn't check for ACLs
            if ( RT->Config->Get( 'StrictLinkACL' ) ) {
                if ( $obj && !$obj->CurrentUserHasRight('ModifyAsset') ) {
                    push @errors, $self->loc('Linking. Permission denied');
                    next;
                }
            }

            if ( $obj && lc $obj->Status eq 'deleted' ) {
                push @errors,
                  $self->loc("Linking. Can't link to a deleted asset");
                next;
            }

            my ( $wval, $wmsg ) = $self->_AddLink(
                Type                          => $LINKTYPEMAP{$type}->{'Type'},
                $LINKTYPEMAP{$type}->{'Mode'} => $link,
                Silent                        => !$args{'_RecordTransaction'},
                'Silent'. ( $LINKTYPEMAP{$type}->{'Mode'} eq 'Base'? 'Target': 'Base' )
                                              => 1,
            );

            push @errors, $wmsg unless ($wval);
        }
    }

    return (@errors ? 0 : 1), \@errors;
}



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

        #Can't create an asset without a type.
        unless ( $TypeObj->Id ) {
            return ( 0, 0, $self->loc("Could not load asset type '[_1]'", $args{Type}) );
        }
    
        #Now that we have a type, Check the ACLS
        unless ( $self->CurrentUser->HasRight(
                Right  => 'CreateAsset',
                Object => $TypeObj,
            )
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
    foreach my $type ( RTx::AssetTracker::Type->RoleGroupTypes() ) {
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

            return $wval, 0, $wmsg unless ($wval);
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

            return $wval, 0, $wmsg unless ($wval);
        }
    }

    #custom fields
    foreach my $arg ( keys %args ) {
        next unless ( $arg =~ /^CustomField-(\d+)$/i );
        my $cfid = $1;

        my $values_new =  ref( $args{$arg} ) eq 'ARRAY' ? $args{$arg} : [ $args{$arg} ];
        my $values_current = [  map { $_->Content } @{ $self->CustomFieldValues($cfid)->ItemsArrayRef }];

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
            return $rv, 0, $msg unless $rv;

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
    return $addlink_rv, 0, join("\n", @$addlink_errors) unless $addlink_rv;

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




=head2 _CreateAssetGroups

Create the asset groups and links for this asset.
This routine expects to be called from Asset->Create _inside of a transaction_

It will create two groups for this asset: Admin and Owner.

It will return true on success and undef on failure.

=cut


sub _CreateAssetGroups {
    my $self = shift;
    my %args = @_;

    my @types = RTx::AssetTracker::Type->RoleGroupTypes;

    foreach my $type (@types) {
        my $type_obj = RT::Group->new($self->CurrentUser);
        my ($id, $msg) = $type_obj->_Create(Domain => 'RTx::AssetTracker::Asset-Role',
                                                       Instance => $self->Id,
                                                       Type => $type,
                                                       InsideTransaction => 1,
                                                       '_RecordTransaction' => $args{'_Commit'} && $args{'_RecordTransaction'} );
        unless ($id) {
            $RT::Logger->error("Couldn't create an asset group of type '$type' for asset ".
                               $self->Id.": ".$msg);
            return(undef);
        }
     }
    return(1);

}



=head2 AddWatcher

AddWatcher takes a parameter hash. The keys are as follows:

Type        One of Asset->RoleGroupTypes

PrinicpalId The RT::Principal id of the user or group that's being added as a watcher

Email       The email address of the new watcher. If a user with this
            email address can't be found, a new nonprivileged user will be created.

If the watcher you're trying to set has an RT account, set the PrincipalId paremeter to their User Id. Otherwise, set the Email parameter to their Email address.

=cut

sub AddWatcher {
    my $self = shift;
    my %args = (
        Type  => undef,
        PrincipalId => undef,
        Email => undef,
        @_
    );

    # ModifyAsset works in any case
    return $self->_AddWatcher( %args )
        if $self->CurrentUserHasRight('ModifyAsset');
    if ( $args{'Email'} ) {
        my ($addr) = RT::EmailParser->ParseEmailAddress( $args{'Email'} );
        return (0, $self->loc("Couldn't parse address from '[_1]' string", $args{'Email'} ))
            unless $addr;

        if ( lc $self->CurrentUser->EmailAddress
            eq lc RT::User->CanonicalizeEmailAddress( $addr->address ) )
        {
            $args{'PrincipalId'} = $self->CurrentUser->id;
            delete $args{'Email'};
        }
    }

    # If the watcher isn't the current user then the current user has no right
    # bail
    unless ( $args{'PrincipalId'} && $self->CurrentUser->id == $args{'PrincipalId'} ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    #  If they don't have RoleRight for this role, bail
    if ( $args{'Type'} ) {
        unless ( $self->CurrentUserHasRight(RTx::AssetTracker::Type->RoleRight($args{'Type'})) ) {
            return ( 0, $self->loc('Permission Denied') );
        }
    }
    else {
        $RT::Logger->warning( "AddWatcher got passed a bogus type");
        return ( 0, $self->loc('Error in parameters to Asset->AddWatcher') );
    }

    return $self->_AddWatcher( %args );
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
        if ( RT::EmailParser->IsRTAddress( $args{'Email'} ) ) {
            return (0, $self->loc("[_1] is an address RT receives mail at. Adding it as a '[_2]' would create a mail loop", $args{'Email'}, $self->loc($args{'Type'})));
        }
        my $user = RT::User->new(RT->SystemUser);
        $user->LoadByEmail($args{'Email'});
        $args{'PrincipalId'} = $user->PrincipalId if $user->Id;
    }
    elsif ($args{'Name'}) {
        my $user = RT::User->new(RT->SystemUser);
        $user->Load($args{'Name'});
        $args{'PrincipalId'} = $user->PrincipalId if $user->Id;
    }
    if ($args{'PrincipalId'}) {
        $principal->Load($args{'PrincipalId'});
        if ( $principal->id and $principal->IsUser and my $email = $principal->Object->EmailAddress ) {
            return (0, $self->loc("[_1] is an address RT receives mail at. Adding it as a '[_2]' would create a mail loop", $email, $self->loc($args{'Type'})))
                if RT::EmailParser->IsRTAddress( $email );

        }
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

        return ( 0, $self->loc('[_1] is already a [_2] for this asset',
                    $principal->Object->Name, $self->loc($args{'Type'})) );
    }


    my ( $m_id, $m_msg ) = $group->_AddMember( PrincipalId => $principal->Id,
                                               InsideTransaction => 1 );
    unless ($m_id) {
        $RT::Logger->error("Failed to add ".$principal->Id." as a member of group ".$group->Id.": ".$m_msg);

        return ( 0, $self->loc('Could not make [_1] a [_2] for this asset',
                    $principal->Object->Name, $self->loc($args{'Type'})) );
    }

    unless ( $args{'Silent'} ) {
        $self->_NewTransaction(
            Type     => 'AddWatcher',
            NewValue => $principal->Id,
            Field    => $args{'Type'},
            Data     => $args{TransactionData},
        );
    }

    return ( 1, $self->loc('Added [_1] as a [_2] for this asset',
                $principal->Object->Name, $self->loc($args{'Type'})) );
}




=head2 DeleteWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID, Email => EMAIL_ADDRESS }


Deletes an Asset watcher.  Takes two arguments:

Type  (one of Asset->RoleGroupTypes)

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
        my $user = RT::User->new( $self->CurrentUser );
        $user->LoadByEmail( $args{'Email'} );
        $principal->Load( $user->Id );
    }
    elsif ($args{'Name'}) {
        my $user = RT::User->new( $self->CurrentUser );
        $user->Load( $args{'Name'} );
        $principal->Load( $user->Id );
    }

    # If we can't find this watcher, we need to bail.
    unless ( $principal->Id ) {
        return ( 0, $self->loc("Could not find that principal") );
    }

    my $group = $self->LoadAssetRoleGroup( Type => $args{'Type'} );
    unless ( $group->id ) {
        return ( 0, $self->loc("Group not found") );
    }

    # Check ACLS
    #If the watcher we're trying to add is for the current user
    if ( $self->CurrentUser->PrincipalId == $principal->id ) {

        #  If they don't have 'RoleRight' for this role
        #   or 'ModifyAsset', bail
        if ( $args{'Type'} ) {
            unless (    $self->CurrentUserHasRight('ModifyAsset')
                     or $self->CurrentUserHasRight(RTx::AssetTracker::Type->RoleRight($args{'Type'})) ) {
                return ( 0, $self->loc('Permission Denied') );
            }
        }
        else {
            $RT::Logger->warning("$self -> DeleteWatcher got passed a bogus type");
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
                 $self->loc( '[_1] is not a [_2] for this asset',
                             $principal->Object->Name, $args{'Type'} ) );
    }

    my ( $m_id, $m_msg ) = $group->_DeleteMember( $principal->Id );
    unless ($m_id) {
        $RT::Logger->error( "Failed to delete "
                            . $principal->Id
                            . " as a member of group "
                            . $group->Id . ": "
                            . $m_msg );

        return (0,
                $self->loc(
                    'Could not remove [_1] as a [_2] for this asset',
                    $principal->Object->Name, $args{'Type'} ) );
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



# a generic routine to be called by IsOwner and IsAdmin

=head2 IsWatcher { Type => TYPE, PrincipalId => PRINCIPAL_ID, Email => EMAIL }

Takes a param hash with the attributes Type and either PrincipalId or Email

Type is one of Asset->RoleGroupTypes

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

    # Ask if it has the member in question
    return $group->HasMember( $args{'PrincipalId'} );
}



=head2 IsOwner PRINCIPAL_ID

  Takes an RT::Principal id
  Returns true if the principal is a owner of the current asset.

  This method is auto-generated.

=cut


=head2 IsAdmin PRINCIPAL_ID

  Takes an RT::Principal id.
  Returns true if the principal is a requestor of the current asset.

  This method is auto-generated.

=cut



sub SetType {
    my $self = shift;
    my %args = (
        @_
    );

    my $NewType = $args{'Value'};

    #Redundant. ACL gets checked in _Set;
    unless ( $self->CurrentUserHasRight('ModifyAsset') ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    my $NewTypeObj = RTx::AssetTracker::Type->new( $self->CurrentUser );
    $NewTypeObj->Load($NewType);

    unless ( $NewTypeObj->Id() ) {
        return ( 0, $self->loc("That type does not exist") );
    }

    if ( $NewTypeObj->Id == $self->TypeObj->Id ) {
        return ( 0, $self->loc('That is the same value') );
    }
    unless ( $self->CurrentUser->HasRight( Right    => 'CreateAsset', Object => $NewTypeObj)) {
        return ( 0, $self->loc("You may not create assets of that type.") );
    }

    my $new_status;
    my $old_lifecycle = $self->TypeObj->Lifecycle;
    my $new_lifecycle = $NewTypeObj->Lifecycle;
    if ( $old_lifecycle->Name ne $new_lifecycle->Name ) {
        unless ( $old_lifecycle->HasMoveMap( $new_lifecycle ) ) {
            return ( 0, $self->loc("There is no mapping for statuses between these types. Contact your system administrator.") );
        }
        $new_status = $old_lifecycle->MoveMap( $new_lifecycle )->{ lc $self->Status };
        return ( 0, $self->loc("Mapping between types' lifecycles is incomplete. Contact your system administrator.") )
            unless $new_status;
    }

    if ( $new_status ) {
        my $clone = RTx::AssetTracker::Asset->new( RT->SystemUser );
        $clone->Load( $self->Id );
        unless ( $clone->Id ) {
            return ( 0, $self->loc("Couldn't load copy of asset #[_1].", $self->Id) );
        }

        #Actually update the status
        my ($val, $msg)= $clone->_Set(
            Field             => 'Status',
            Value             => $new_status,
            RecordTransaction => 0,
        );
        $RT::Logger->error( 'Status change failed on type change: '. $msg )
            unless $val;
    }

    my ($status, $msg) = $self->_Set( %args, Field => 'Type', Value => $NewTypeObj->Id() );

    if ( $status ) {
        # Clear the type object cache;
        $self->{_type_obj} = undef;
    }

    return ($status, $msg);
}


=head2 TypeObj

Takes nothing. returns this asset's type object

=cut

sub TypeObj {
    my $self = shift;

    if(!$self->{_type_obj} || ! $self->{_type_obj}->id) {

        $self->{_type_obj} = RTx::AssetTracker::Type->new( $self->CurrentUser );

        #We call __Value so that we can avoid the ACL decision and some deep recursion
        my ($result) = $self->{_type_obj}->Load( $self->__Value('Type') );
    }
    return ($self->{_type_obj});
}

sub AssetTypeObj { $_[0]->TypeObj() }



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

    if (RT->Config->Get('GlobalUniqueAssetName')) {
        $Assets->Limit(FIELD => "Name", VALUE => $name);
        $Assets->Limit(FIELD => "id", OPERATOR => "!=", VALUE => $self->Id) if $self->Id;
        return (0, "Asset name $name isn't unique across the entire asset database") if $Assets->Count;
    }
    if (RT->Config->Get('TypeUniqueAssetName')) {
        $Assets->Limit(FIELD => "Type", VALUE => $type);
        $Assets->Limit(FIELD => "id", OPERATOR => "!=", VALUE => $self->Id) if $self->Id;
        return (0, "Asset name $name isn't unique among assets of type: " . $Type->Name) if $Assets->Count;
    }
    if (RT->Config->Get('TypeStatusUniqueAssetName')) {
        $Assets->Limit(FIELD => "Status", VALUE => $stat);
        $Assets->Limit(FIELD => "id", OPERATOR => "!=", VALUE => $self->Id) if $self->Id;
        return (0, "Asset name $name isn't unique among assets of type: " . $Type->Name . ", and status: $stat") if $Assets->Count;
    }

    return 1, "Asset name satifies uniqueness.";

}



sub SetDescription {
    my $self = shift;
           
    $self->_SetBasic(@_, Field => 'Description');
}


sub _Links {
    my $self = shift;

    #TODO: Field isn't the right thing here. but I ahave no idea what mnemonic ---
    #tobias meant by $f
    my $field = shift;
    my $type  = shift || "";

    my $cache_key = "$field$type";
    return $self->{ $cache_key } if $self->{ $cache_key };

    my $links = $self->{ $cache_key }
              = RT::Links->new( $self->CurrentUser );
    unless ( $self->CurrentUserHasRight('ShowAsset') ) {
        $links->Limit( FIELD => 'id', VALUE => 0, SUBCLAUSE => 'acl' );
        return $links;
    }

    # at least to myself
    $links->Limit(
        FIELD => $field,
        VALUE => $self->URI );
    $links->Limit(
        FIELD => 'Type',
        VALUE => $type,
    ) if $type;

    return $links;
}



=head2 DeleteLink

Delete a link. takes a paramhash of Base, Target, Type, Silent,
SilentBase and SilentTarget. Either Base or Target must be null.
The null value will be replaced with this asset's id.

If Silent is true then no transaction would be recorded, in other
case you can control creation of transactions on both base and
target with SilentBase and SilentTarget respectively. By default
both transactions are created.

=cut

sub DeleteLink {
    my $self = shift;
    my %args = (
        Base         => undef,
        Target       => undef,
        Type         => undef,
        Silent       => undef,
        SilentBase   => undef,
        SilentTarget => undef,
        TransactionData => undef,
        @_
    );

    unless ( $args{'Target'} || $args{'Base'} ) {
        $RT::Logger->error("Base or Target must be specified");
        return ( 0, $self->loc('Either base or target must be specified') );
    }

    #check acls
    my $right = 0;
    $right++ if $self->CurrentUserHasRight('ModifyAsset');
    if ( !$right && RT->Config->Get( 'ModifyBothAssetsForLink' ) ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    # If the other URI is an RTx::AssetTracker::Asset, we want to make sure the user
    # can modify it too...
    my ($status, $msg, $other_asset) = $self->__GetAssetFromURI( URI => $args{'Target'} || $args{'Base'} );
    return (0, $msg) unless $status;
    if ( !$other_asset || $other_asset->CurrentUserHasRight('ModifyAsset') ) {
        $right++;
    }
    if ( ( !RT->Config->Get( 'ModifyBothAssetsForLink' ) && $right == 0 ) ||
         ( RT->Config->Get( 'ModifyBothAssetsForLink' ) && $right < 2 ) )
    {
        return ( 0, $self->loc("Permission Denied") );
    }

    my ($val, $Msg) = $self->SUPER::_DeleteLink(%args);
    return ( 0, $Msg ) unless $val;

    return ( $val, $Msg ) if $args{'Silent'};

    my ($direction, $remote_link);

    if ( $args{'Base'} ) {
        $remote_link = $args{'Base'};
        $direction = 'Target';
    }
    elsif ( $args{'Target'} ) {
        $remote_link = $args{'Target'};
        $direction = 'Base';
    }

    my $remote_uri = RT::URI->new( $self->CurrentUser );
    $remote_uri->FromURI( $remote_link );

    unless ( $args{ 'Silent'. $direction } ) {
        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
            Type      => 'DeleteLink',
            Field     => $LINKDIRMAP{$args{'Type'}}->{$direction},
            OldValue  => $remote_uri->URI || $remote_link,
            TimeTaken => 0,
            Data      => $args{TransactionData},
        );
        $RT::Logger->error("Couldn't create transaction: $Msg") unless $Trans;
    }

    if ( !$args{ 'Silent'. ( $direction eq 'Target'? 'Base': 'Target' ) } && $remote_uri->IsLocal ) {
        my $OtherObj = $remote_uri->Object;
        my ( $val, $Msg ) = $OtherObj->_NewTransaction(
            Type           => 'DeleteLink',
            Field          => $direction eq 'Target' ? $LINKDIRMAP{$args{'Type'}}->{Base}
                                            : $LINKDIRMAP{$args{'Type'}}->{Target},
            OldValue       => $self->URI,
            ActivateScrips => !RT->Config->Get('LinkTransactionsRun1Scrip'),
            TimeTaken      => 0,
        );
        $RT::Logger->error("Couldn't create transaction: $Msg") unless $val;
    }

    return ( $val, $Msg );
}



=head2 AddLink

Takes a paramhash of Type and one of Base or Target. Adds that link to this asset.

If Silent is true then no transaction would be recorded, in other
case you can control creation of transactions on both base and
target with SilentBase and SilentTarget respectively. By default
both transactions are created.

=cut

sub AddLink {
    my $self = shift;
    my %args = ( Target       => '',
                 Base         => '',
                 Type         => '',
                 Silent       => undef,
                 SilentBase   => undef,
                 SilentTarget => undef,
                 TransactionData => undef,
                 @_ );

    unless ( $args{'Target'} || $args{'Base'} ) {
        $RT::Logger->error("Base or Target must be specified");
        return ( 0, $self->loc('Either base or target must be specified') );
    }

    my $right = 0;
    $right++ if $self->CurrentUserHasRight('ModifyAsset');
    if ( !$right && RT->Config->Get( 'ModifyBothAssetsForLink' ) ) {
        return ( 0, $self->loc("Permission Denied") );
    }

    # If the other URI is an RTx::AssetTracker::Asset, we want to make sure the user
    # can modify it too...
    my ($status, $msg, $other_asset) = $self->__GetAssetFromURI( URI => $args{'Target'} || $args{'Base'} );
    return (0, $msg) unless $status;
    if ( !$other_asset || $other_asset->CurrentUserHasRight('ModifyAsset') ) {
        $right++;
    }
    if ( ( !RT->Config->Get( 'ModifyBothAssetsForLink' ) && $right == 0 ) ||
         ( RT->Config->Get( 'ModifyBothAssetsForLink' ) && $right < 2 ) )
    {
        return ( 0, $self->loc("Permission Denied") );
    }

    return ( 0, "Can't link to a deleted asset" )
      if $other_asset && lc $other_asset->Status eq 'deleted';

    return $self->_AddLink(%args);
}

sub __GetAssetFromURI {
    my $self = shift;
    my %args = ( URI => '', @_ );

    # If the other URI is an RTx::AssetTracker::Asset, we want to make sure the user
    # can modify it too...
    my $uri_obj = RT::URI->new( $self->CurrentUser );
    unless ($uri_obj->FromURI( $args{'URI'} )) {
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

=head2 _AddLink

Private non-acled variant of AddLink so that links can be added during create.

=cut

sub _AddLink {
    my $self = shift;
    my %args = ( Target       => '',
                 Base         => '',
                 Type         => '',
                 Silent       => undef,
                 SilentBase   => undef,
                 SilentTarget => undef,
                 @_ );

    my ($val, $msg, $exist) = $self->SUPER::_AddLink(%args);
    return ($val, $msg) if !$val || $exist;
    return ($val, $msg) if $args{'Silent'};

    my ($direction, $remote_link);
    if ( $args{'Target'} ) {
        $remote_link  = $args{'Target'};
        $direction    = 'Base';
    } elsif ( $args{'Base'} ) {
        $remote_link  = $args{'Base'};
        $direction    = 'Target';
    }

    my $remote_uri = RT::URI->new( $self->CurrentUser );
    $remote_uri->FromURI( $remote_link );

    unless ( $args{ 'Silent'. $direction } ) {
        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
            Type      => 'AddLink',
            Field     => $LINKDIRMAP{$args{'Type'}}->{$direction},
            NewValue  =>  $remote_uri->URI || $remote_link,
            TimeTaken => 0,
            Data      => $args{TransactionData},
        );
        $RT::Logger->error("Couldn't create transaction: $Msg") unless $Trans;
    }

    if ( !$args{ 'Silent'. ( $direction eq 'Target'? 'Base': 'Target' ) } && $remote_uri->IsLocal ) {
        my $OtherObj = $remote_uri->Object;
        my ( $val, $msg ) = $OtherObj->_NewTransaction(
            Type           => 'AddLink',
            Field          => $direction eq 'Target' ? $LINKDIRMAP{$args{'Type'}}->{Base}
                                            : $LINKDIRMAP{$args{'Type'}}->{Target},
            NewValue       => $self->URI,
            ActivateScrips => !RT->Config->Get('LinkTransactionsRun1Scrip'),
            TimeTaken      => 0,
        );
        $RT::Logger->error("Couldn't create transaction: $msg") unless $val;
    }

    return ( $val, $msg );
}




=head2 SetStatus STATUS

Set this asset's status.

Alternatively, you can pass in a list of named parameters (Status => STATUS).

=cut

sub SetStatus {
    my $self = shift;
    my %args;
    if (@_ == 1) {
        $args{Status} = shift;
    }
    else {
        %args = (@_);
    }

    $args{Status} = $args{Status} || $args{Value};


    my $lifecycle = $self->TypeObj->Lifecycle;

    my $new = lc $args{'Status'};
    unless ( $lifecycle->IsValid( $new ) ) {
        return (0, $self->loc("Status '[_1]' isn't a valid status for assets of this type.", $self->loc($new)));
    }

    my $old = $self->__Value('Status');
    unless ( $lifecycle->IsTransition( $old => $new ) ) {
        return (0, $self->loc("You can't change status from '[_1]' to '[_2]'.", $self->loc($old), $self->loc($new)));
    }

    my $check_right = $lifecycle->CheckRight( $old => $new );
    unless ( $self->CurrentUserHasRight( $check_right ) ) {
        return ( 0, $self->loc('Permission Denied') );
    }

    if ( $lifecycle->IsInactive( $new ) ) {
        # We don't want inactive assets to have IP addresses
        my $ips = $self->IPs();
        while (my $ip = $ips->Next) {
            my($a, $b) = $self->DeleteIP( IP => $ip->IP );
        }
    }

    #Actually update the status
    my ($val, $msg)= $self->_Set(
        %args,
        Field           => 'Status',
        Value           => $new,
        TimeTaken       => 0,
        CheckACL        => 0,
        TransactionType => 'Status',
    );
    return ($val, $msg);
}



=head2 Delete

Takes no arguments. Marks this ticket for garbage collection

=cut

sub Delete {
    my $self = shift;
    unless ( $self->TypeObj->Lifecycle->IsValid('deleted') ) {
        return (0, $self->loc('Delete operation is disabled by lifecycle configuration') ); #loc
    }
    return ( $self->SetStatus('deleted') );
}


sub _Set {
    my $self = shift;

    my %args = ( Field             => undef,
                 Value             => undef,
                 TimeTaken         => 0,
                 RecordTransaction => 1,
                 UpdateAsset       => 1,
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
        return ( 0, $msg ) unless $ret;
    }

    if ( $args{'RecordTransaction'} == 1 ) {

        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
                                               Type => $args{'TransactionType'},
                                               Field     => $args{'Field'},
                                               NewValue  => $args{'Value'},
                                               OldValue  => $Old,
                                               TimeTaken => undef,
                                               Data      => $args{TransactionData},
        );
        # Ensure that we can read the transaction, even if the change
        # just made the asset unreadable to us
        $TransObj->{ _object_is_readable } = 1;
        return ( $Trans, scalar $TransObj->BriefDescription );
    }
    else {
        return ( $ret, $msg );
    }
}



=head2 _Value

Takes the name of a table column.
Returns its value as a string, if the user passes an ACL check

=cut

sub _Value {

    my $self  = shift;
    my $field = shift;

    #if the field is public, return it.
    if ( $self->_Accessible( $field, 'public' ) ) {

        #$RT::Logger->debug("Skipping ACL check for $field");
        return ( $self->SUPER::_Value($field) );

    }

    #If the current user doesn't have ACLs, don't let em at it.

    unless ( $self->CurrentUserHasRight('ShowAsset') ) {
        return (undef);
    }
    return ( $self->SUPER::_Value($field) );

}



=head2 CurrentUserHasRight

  Takes the textual name of an Asset scoped right (from RT::ACE) and returns
1 if the user has that right. It returns 0 if the user doesn't have that right.

=cut

sub CurrentUserHasRight {
    my $self  = shift;
    my $right = shift;

    return $self->CurrentUser->PrincipalObj->HasRight(
        Object => $self,
        Right  => $right,
    )
}


=head2 CurrentUserCanSee

Returns true if the current user can see the asset, using ShowAsset

=cut

sub CurrentUserCanSee {
    my $self = shift;
    return $self->CurrentUserHasRight('ShowAsset');
}

=head2 HasRight

 Takes a paramhash with the attributes 'Right' and 'Principal'
  'Right' is an asset-scoped textual right from RT::ACE
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
        Carp::cluck("Principal attrib undefined for Asset::HasRight");
        $RT::Logger->crit("Principal attrib undefined for Asset::HasRight");
        return(undef);
    }

    return (
        $args{'Principal'}->HasRight(
            Object => $self,
            Right     => $args{'Right'}
          )
    );
}



=head2 CustomFieldLookupType

Returns the RTx::AssetTracker::Asset lookup type, which can be passed to 
RT::CustomField->Create() via the 'LookupType' hash key.

=cut


sub CustomFieldLookupType {
    "RTx::AssetTracker::Type-RTx::AssetTracker::Asset";
}


=head2 ACLEquivalenceObjects

This method returns a list of objects for which a user's rights also apply
to this ticket. Generally, this is only the ticket's queue, but some RT 
extensions may make other objects available too.

This method is called from L<RT::Principal/HasRight>.

=cut

sub ACLEquivalenceObjects {
    my $self = shift;
    return $self->TypeObj;

}


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
        $cf->LoadByNameAndAssetType( Name => $field, Type => $self->TypeObj->Id );
        unless ( $cf->id ) {
            $cf->LoadByNameAndAssetType( Name => $field, Type => '0' );
        }
        $field = $cf->id;
        unless ($field) { return RT::CustomFieldValues->new( $self->CurrentUser ); }
    }
    return $self->SUPER::CustomFieldValues($field);
}



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

    unless ( $self->CurrentUserHasRight('ModifyAsset') ||
             $self->CurrentUserHasRight('RetireAsset') ||
             $self->CurrentUserHasRight('DeleteAsset') ) {
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



=head2 LoadAssetRoleGroup  { Type => TYPE }

Loads an asset group from the database.

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



=head2 id

Returns the current value of id. 
(In the database, id is stored as int(11).)


=cut


=head2 Type

Returns the current value of Type. 
(In the database, Type is stored as int(11).)



=head2 SetType VALUE


Set Type to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Type will be stored as a int(11).)


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


=head2 Status

Returns the current value of Status. 
(In the database, Status is stored as varchar(64).)



=head2 SetStatus VALUE


Set Status to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Status will be stored as a varchar(64).)


=cut


=head2 LastUpdatedBy

Returns the current value of LastUpdatedBy. 
(In the database, LastUpdatedBy is stored as int(11).)


=cut


=head2 LastUpdated

Returns the current value of LastUpdated. 
(In the database, LastUpdated is stored as datetime.)


=cut


=head2 Creator

Returns the current value of Creator. 
(In the database, Creator is stored as int(11).)


=cut


=head2 Created

Returns the current value of Created. 
(In the database, Created is stored as datetime.)


=cut


=head2 URI

Returns the current value of URI. 
(In the database, URI is stored as varchar(255).)



=head2 SetURI VALUE


Set URI to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, URI will be stored as a varchar(255).)


=cut



sub _CoreAccessible {
    {
     
        id =>
		{read => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => ''},
        Type => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Name => 
		{read => 1, write => 1, sql_type => 12, length => 200,  is_blob => 0,  is_numeric => 0,  type => 'varchar(200)', default => ''},
        Description => 
		{read => 1, write => 1, sql_type => 12, length => 255,  is_blob => 0,  is_numeric => 0,  type => 'varchar(255)', default => ''},
        Status => 
		{read => 1, write => 1, sql_type => 12, length => 64,  is_blob => 0,  is_numeric => 0,  type => 'varchar(64)', default => ''},
        LastUpdatedBy => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        LastUpdated => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        Creator => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Created => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        URI => 
		{read => 1, write => 1, sql_type => 12, length => 255,  is_blob => 0,  is_numeric => 0,  type => 'varchar(255)', default => ''},

 }
};

RT::Base->_ImportOverlays();

1;
