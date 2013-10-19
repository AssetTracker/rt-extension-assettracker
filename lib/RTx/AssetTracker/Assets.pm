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

  RTx::AssetTracker::Assets - a collection of AssetTracker Assets objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Assets;

=head1 DESCRIPTION


=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Assets;
use base 'RT::SearchBuilder';

use RTx::AssetTracker::Asset;

sub Table {'AT_Assets'};

# most logic "borrowed" from RT::Tickets rev 2814

use vars qw( %FIELDS );
use RT::CustomFields;
use File::Temp 'tempdir';
use HTML::Mason;
use XML::Parser;

# Configuration Tables:

# FIELD_METADATA is a mapping of searchable Field name, to Type, and other
# metadata.

our %FIELD_METADATA = (
    Name             => [ 'STRING', ],
    Description      => [ 'STRING', ],
    Status           => [ 'ENUM' ],
    Type             => [ 'ENUM' => 'Type', ],
    Creator          => [ 'ENUM' => 'RT::User', ],
    LastUpdatedBy    => [ 'ENUM' => 'RT::User', ],
    id               => [ 'INT', ],
    URI              => [ 'STRING', ],

    Linked           => [ 'LINK' ], #loc_left_pair
    LinkedTo         => [ 'LINK' => 'To' ], #loc_left_pair
    LinkedFrom       => [ 'LINK' => 'From' ], #loc_left_pair
    LastUpdated      => [ 'DATE' => 'LastUpdated', ],
    Created          => [ 'DATE' => 'Created', ],
    TransactionData  => [ 'TRANSFIELD' => 'Data', ],
    IP               => [ 'IPFIELD', ],
    MAC              => [ 'IPFIELD', ],
    Interface        => [ 'IPFIELD', ],
    Port             => [ 'PORTFIELD', ],
    Transport        => [ 'PORTFIELD', ],
    TransactionDate  => [ 'TRANSDATE', ],
    Updated          => [ 'TRANSDATE', ],
    Watcher          => [ 'WATCHERFIELD' ],
    TypeWatcher      => [ 'WATCHERFIELD'    => undef   => 'Type', ],
    CustomFieldValue => [ 'CUSTOMFIELD' => 'Asset' ], #loc_left_pair
    CustomField      => [ 'CUSTOMFIELD' => 'Asset' ], #loc_left_pair
    CF               => [ 'CUSTOMFIELD' => 'Asset' ], #loc_left_pair
    WatcherGroup     => [ 'MEMBERSHIPFIELD', ],
  );

# Lower Case version of FIELDS, for case insensitivity
our %LOWER_CASE_FIELDS = map { ( lc($_) => $_ ) } (keys %FIELD_METADATA);

our %SEARCHABLE_SUBFIELDS = (
    User => [qw(
        EmailAddress Name RealName Nickname Organization Address1 Address2
        WorkPhone HomePhone MobilePhone PagerPhone id
    )],
);

# Mapping of Field Type to Function
our %dispatch = (
    ENUM            => \&_EnumLimit,
    INT             => \&_IntLimit,
    LINK            => \&_LinkLimit,
    DATE            => \&_DateLimit,
    STRING          => \&_StringLimit,
    TRANSFIELD      => \&_TransLimit,
    TRANSDATE       => \&_TransDateLimit,
    WATCHERFIELD    => \&_WatcherLimit,
    MEMBERSHIPFIELD => \&_WatcherMembershipLimit,
    CUSTOMFIELD     => \&_CustomFieldLimit,
    IPFIELD         => \&_IPLimit,
    PORTFIELD       => \&_PortLimit,
);
our %can_bundle = ();# WATCHERFIELD => "yes", );

# Default EntryAggregator per type
# if you specify OP, you must specify all valid OPs
my %DefaultEA = (
    INT  => 'AND',
    ENUM => {
        '='  => 'OR',
        '!=' => 'AND'
    },
    DATE => {
        '='  => 'OR',
        '>=' => 'AND',
        '<=' => 'AND',
        '>'  => 'AND',
        '<'  => 'AND'
    },
    STRING => {
        '='        => 'OR',
        '!='       => 'AND',
        'LIKE'     => 'AND',
        'NOT LIKE' => 'AND'
    },
    TRANSFIELD   => 'AND',
    TRANSDATE    => 'AND',
    LINK         => 'OR',
    LINKFIELD    => 'AND',
    TARGET       => 'AND',
    BASE         => 'AND',
    WATCHERFIELD => {
        '='        => 'OR',
        '!='       => 'AND',
        'LIKE'     => 'OR',
        'NOT LIKE' => 'AND'
    },

    CUSTOMFIELD => 'OR',
    IPFIELD     => 'OR',
);

# Helper functions for passing the above lexically scoped tables above
# into Assets_SQL.
sub FIELDS     { return \%FIELD_METADATA }
sub dispatch   { return \%dispatch }
sub can_bundle { return \%can_bundle }

# Bring in the clowns.
require RTx::AssetTracker::Assets_SQL;


our @SORTFIELDS = qw(id Status
    Type Name Description
    Created LastUpdated);

=head2 SortFields

Returns the list of fields that lists of assets can easily be sorted by

=cut

sub SortFields {
    my $self = shift;
    return (@SORTFIELDS);
}


# BEGIN SQL STUFF *********************************


sub CleanSlate {
    my $self = shift;
    $self->SUPER::CleanSlate( @_ );
    delete $self->{$_} foreach qw(
        _sql_cf_alias
        _sql_group_members_aliases
        _sql_object_cfv_alias
        _sql_role_group_aliases
        _sql_u_watchers_alias_for_sort
        _sql_u_watchers_aliases
        _sql_current_user_can_see_applied
        _sql_ipalias
        _sql_portalias
    );
}

=head1 Limit Helper Routines

These routines are the targets of a dispatch table depending on the
type of field.  They all share the same signature:

  my ($self,$field,$op,$value,@rest) = @_;

The values in @rest should be suitable for passing directly to
DBIx::SearchBuilder::Limit.

Essentially they are an expanded/broken out (and much simplified)
version of what ProcessRestrictions used to do.  They're also much
more clearly delineated by the TYPE of field being processed.

=head2 _EnumLimit

Handle Fields which are limited to certain values, and potentially
need to be looked up from another class.

This subroutine actually handles two different kinds of fields.  For
some the user is responsible for limiting the values.  (i.e. Status,
Type).

For others, the value specified by the user will be looked by via
specified class.

Meta Data:
  name of class to lookup in (Optional)

=cut

sub _EnumLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    # SQL::Statement changes != to <>.  (Can we remove this now?)
    $op = "!=" if $op eq "<>";

    die "Invalid Operation: $op for $field"
        unless $op eq "="
        or $op     eq "!=";

    my $meta = $FIELD_METADATA{$field};
    if ( defined $meta->[1] && defined $value && $value !~ /^\d+$/ ) {
        my $class = ( $meta->[1] =~ /::/ ? '' : "RTx::AssetTracker::" ) . $meta->[1];
        my $o     = $class->new( $sb->CurrentUser );
        $o->Load($value);
        $value = $o->Id || 0;
    } elsif ($field eq "Status") {
        $value = lc $value;
    }
    $sb->_SQLLimit(
        FIELD    => $field,
        VALUE    => $value,
        OPERATOR => $op,
        @rest,
    );
}

=head2 _IntLimit

Handle fields where the values are limited to integers.  (For example,
Priority, TimeWorked.)

Meta Data:
  None

=cut

sub _IntLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    die "Invalid Operator $op for $field"
        unless $op =~ /^(=|!=|>|<|>=|<=)$/;

    $sb->_SQLLimit(
        FIELD    => $field,
        VALUE    => $value,
        OPERATOR => $op,
        @rest,
    );
}

=head2 _LinkLimit

Handle fields which deal with links between assets.

Meta Data:
  1: Direction (From, To)
  2: Link Type (one of 'AssetLinkTypes')

=cut

sub _LinkLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    my $meta = $FIELD_METADATA{$field};
    die "Invalid Operator $op for $field" unless $op =~ /^(=|!=|IS|IS NOT)$/io;

    my $is_negative = 0;
    if ( $op eq '!=' || $op =~ /\bNOT\b/i ) {
        $is_negative = 1;
    }
    my $is_null = 0;
    $is_null = 1 if !$value || $value =~ /^null$/io;

    unless ($is_null) {
        $value = RT::URI->new( $sb->CurrentUser )->CanonicalizeURI( $value );
    }

    my $direction = $meta->[1] || '';
    my ($matchfield, $linkfield) = ('', '');
    if ( $direction eq 'To' ) {
        ($matchfield, $linkfield) = ("Target", "Base");
    }
    elsif ( $direction eq 'From' ) {
        ($matchfield, $linkfield) = ("Base", "Target");
    }
    elsif ( $direction ) {
        die "Invalid link direction '$direction' for $field\n";
    } else {
        $sb->_OpenParen;
        $sb->_LinkLimit( 'LinkedTo', $op, $value, @rest );
        $sb->_LinkLimit(
            'LinkedFrom', $op, $value, @rest,
            ENTRYAGGREGATOR => (($is_negative && $is_null) || (!$is_null && !$is_negative))? 'OR': 'AND',
        );
        $sb->_CloseParen;
        return;
    }

    my $is_local = 1;
    if ( $is_null ) {
        $op = ($op =~ /^(=|IS)$/i)? 'IS': 'IS NOT';
    }
    elsif ( $value =~ /\D/ ) {
        $is_local = 0;
    }
    $matchfield = "Local$matchfield" if $is_local;

#For doing a left join to find "unlinked assets" we want to generate a query that looks like this
#    SELECT main.* FROM AT_Assets main
#        LEFT JOIN Links Links_1 ON (     (Links_1.Type = 'MemberOf')
#                                      AND(main.URI = Links_1.Target))
#        WHERE Links_1.Base IS NULL;

    if ( $is_null ) {
        my $linkalias = $sb->Join(
            TYPE   => 'LEFT',
            ALIAS1 => 'main',
            FIELD1 => 'URI',
            TABLE2 => 'Links',
            FIELD2 => $linkfield
        );
        $sb->Limit(
            LEFTJOIN => $linkalias,
            FIELD    => 'Type',
            OPERATOR => '=',
            VALUE    => $meta->[2],
        ) if $meta->[2];
        $sb->_SQLLimit(
            @rest,
            ALIAS      => $linkalias,
            FIELD      => $matchfield,
            OPERATOR   => $op,
            VALUE      => 'NULL',
            QUOTEVALUE => 0,
        );
    }
    else {
        my $linkalias = $sb->Join(
            TYPE   => 'LEFT',
            ALIAS1 => 'main',
            FIELD1 => 'URI',
            TABLE2 => 'Links',
            FIELD2 => $linkfield
        );
        $sb->Limit(
            LEFTJOIN => $linkalias,
            FIELD    => 'Type',
            OPERATOR => '=',
            VALUE    => $meta->[2],
        ) if $meta->[2];
        $sb->Limit(
            LEFTJOIN => $linkalias,
            FIELD    => $matchfield,
            OPERATOR => '=',
            VALUE    => $value,
        );
        $sb->_SQLLimit(
            @rest,
            ALIAS      => $linkalias,
            FIELD      => $matchfield,
            OPERATOR   => $is_negative? 'IS': 'IS NOT',
            VALUE      => 'NULL',
            QUOTEVALUE => 0,
        );
    }
}

=head2 _DateLimit

Handle date fields.  (Created, LastTold..)

Meta Data:
  1: type of link.  (Probably not necessary.)

=cut

sub _DateLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    die "Invalid Date Op: $op"
        unless $op =~ /^(=|>|<|>=|<=)$/;

    my $meta = $FIELD_METADATA{$field};
    die "Incorrect Meta Data for $field"
        unless ( defined $meta->[1] );

    my $date = RT::Date->new( $sb->CurrentUser );
    $date->Set( Format => 'unknown', Value => $value );

    if ( $op eq "=" ) {

        # if we're specifying =, that means we want everything on a
        # particular single day.  in the database, we need to check for >
        # and < the edges of that day.

        $date->SetToMidnight( Timezone => 'server' );
        my $daystart = $date->ISO;
        $date->AddDay;
        my $dayend = $date->ISO;

        $sb->_OpenParen;

        $sb->_SQLLimit(
            FIELD    => $meta->[1],
            OPERATOR => ">=",
            VALUE    => $daystart,
            @rest,
        );

        $sb->_SQLLimit(
            FIELD    => $meta->[1],
            OPERATOR => "<",
            VALUE    => $dayend,
            @rest,
            ENTRYAGGREGATOR => 'AND',
        );

        $sb->_CloseParen;

    }
    else {
        $sb->_SQLLimit(
            FIELD    => $meta->[1],
            OPERATOR => $op,
            VALUE    => $date->ISO,
            @rest,
        );
    }
}

=head2 _StringLimit

Handle simple fields which are just strings.  (Subject,Type)

Meta Data:
  None

=cut

sub _StringLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    # FIXME:
    # Valid Operators:
    #  =, !=, LIKE, NOT LIKE
    if ( RT->Config->Get('DatabaseType') eq 'Oracle'
        && (!defined $value || !length $value)
        && lc($op) ne 'is' && lc($op) ne 'is not'
    ) {
        if ($op eq '!=' || $op =~ /^NOT\s/i) {
            $op = 'IS NOT';
        } else {
            $op = 'IS';
        }
        $value = 'NULL';
    }

    $sb->_SQLLimit(
        FIELD         => $field,
        OPERATOR      => $op,
        VALUE         => $value,
        CASESENSITIVE => 0,
        @rest,
    );
}

=head2 _IPLimit

Handle fields limiting based on IP

Meta Data:
  None

=cut

sub _IPLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    # See the comments for TransLimit, they apply here too

    $sb->{_sql_ipalias} = $sb->NewAlias('AT_IPs')
        unless defined $sb->{_sql_ipalias};

    #$sb->_OpenParen;

    $sb->_SQLLimit(
        ALIAS    => $sb->{_sql_ipalias},
        FIELD    => $field,
        OPERATOR => $op,
        VALUE    => $value,
        @rest
    );

    # Join IPs to Assets
    $sb->_SQLJoin(
        ALIAS1 => 'main',
        FIELD1 => $sb->{'primary_key'}, # UGH!
        ALIAS2 => $sb->{_sql_ipalias},
        FIELD2 => 'Asset'
    );

    #$sb->_CloseParen;
}

=head2 _PortLimit

Handle fields limiting based on Port

Meta Data:
  None

=cut

sub _PortLimit {

    my ( $self, $field, $op, $value, @rest ) = @_;

    # See the comments for TransLimit, they apply here too

    $self->{_sql_ipalias} = $self->NewAlias('AT_IPs')
      unless defined $self->{_sql_ipalias};

    $self->{_sql_portalias} = $self->NewAlias('AT_Ports')
      unless defined $self->{_sql_portalias};

    $self->_OpenParen;

    #Search for the right field
    $self->_SQLLimit(
        ALIAS         => $self->{_sql_portalias},
        FIELD         => $field,
        OPERATOR      => $op,
        VALUE         => $value,
        CASESENSITIVE => 0,
        @rest
    );

    # Join Ports to IPs
    $self->_SQLJoin(
        ALIAS1 => $self->{_sql_portalias},
        FIELD1 => 'IP',
        ALIAS2 => $self->{_sql_ipalias},
        FIELD2 => 'id',
    );

    # Join IPs to Assets
    $self->_SQLJoin(
        ALIAS1 => 'main',
        FIELD1 => 'id',
        ALIAS2 => $self->{_sql_ipalias},
        FIELD2 => 'Asset'
    );

    $self->_CloseParen;
}



=head2 _TransDateLimit

Handle fields limiting based on Transaction Date.

The inpupt value must be in a format parseable by Time::ParseDate

Meta Data:
  None

=cut

# This routine should really be factored into translimit.
sub _TransDateLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    # See the comments for TransLimit, they apply here too

    my $txn_alias = $sb->JoinTransactions;

    my $date = RT::Date->new( $sb->CurrentUser );
    $date->Set( Format => 'unknown', Value => $value );

    $sb->_OpenParen;
    if ( $op eq "=" ) {

        # if we're specifying =, that means we want everything on a
        # particular single day.  in the database, we need to check for >
        # and < the edges of that day.

        $date->SetToMidnight( Timezone => 'server' );
        my $daystart = $date->ISO;
        $date->AddDay;
        my $dayend = $date->ISO;

        $sb->_SQLLimit(
            ALIAS         => $txn_alias,
            FIELD         => 'Created',
            OPERATOR      => ">=",
            VALUE         => $daystart,
            @rest
        );
        $sb->_SQLLimit(
            ALIAS         => $txn_alias,
            FIELD         => 'Created',
            OPERATOR      => "<=",
            VALUE         => $dayend,
            @rest,
            ENTRYAGGREGATOR => 'AND',
        );

    }

    # not searching for a single day
    else {

        #Search for the right field
        $sb->_SQLLimit(
            ALIAS         => $txn_alias,
            FIELD         => 'Created',
            OPERATOR      => $op,
            VALUE         => $date->ISO,
            @rest
        );
    }

    $sb->_CloseParen;
}

=head2 _TransLimit

Limit based on transaction. (Data, i.e. change comments)

Meta Data:
  1: Field to query on

=cut

sub _TransLimit {
    my ( $self, $field, $op, $value, %rest ) = @_;

    my $meta = $FIELD_METADATA{$field};
    die "Incorrect Meta Data for $field"
        unless ( defined $meta->[1] );

    my $txn_alias = $self->JoinTransactions;

    $self->_SQLLimit(
        %rest,
        ALIAS         => $txn_alias,
        FIELD         => $meta->[1],
        OPERATOR      => $op,
        VALUE         => $value,
        CASESENSITIVE => 0,
    );
}

=head2 _WatcherLimit

Handle watcher limits.  (Requestor, CC, etc..)

Meta Data:
  1: Field to query on



=cut

sub _WatcherLimit {
    my $self  = shift;
    my $field = shift;
    my $op    = shift;
    my $value = shift;
    my %rest  = (@_);

    my $meta = $FIELD_METADATA{ $field };
    my $type = $meta->[1] || '';
    my $class = $meta->[2] || 'Asset';

    # Bail if the subfield is not allowed
    if (    $rest{SUBKEY}
        and not grep { $_ eq $rest{SUBKEY} } @{$SEARCHABLE_SUBFIELDS{'User'}})
    {
        die "Invalid watcher subfield: '$rest{SUBKEY}'";
    }

    # if it's equality op and search by Email or Name then we can preload user
    # we do it to help some DBs better estimate number of rows and get better plans
    if ( $op =~ /^!?=$/ && (!$rest{'SUBKEY'} || $rest{'SUBKEY'} eq 'Name' || $rest{'SUBKEY'} eq 'EmailAddress') ) {
        my $o = RT::User->new( $self->CurrentUser );
        my $method =
            !$rest{'SUBKEY'}
            ? $field eq 'Owner'? 'Load' : 'LoadByEmail'
            : $rest{'SUBKEY'} eq 'EmailAddress' ? 'LoadByEmail': 'Load';
        $o->$method( $value );
        $rest{'SUBKEY'} = 'id';
        $value = $o->id || 0;
    }

    $rest{SUBKEY} ||= 'EmailAddress';

    my ($groups, $group_members, $users);
    if ( $rest{'BUNDLE'} ) {
        ($groups, $group_members, $users) = @{ $rest{'BUNDLE'} };
    } else {
        $groups = $self->_RoleGroupsJoin( Type => $type, Class => $class, New => !$type );
    }

    $self->_OpenParen;
    if ( $op =~ /^IS(?: NOT)?$/i ) {
        # is [not] empty case

        $group_members ||= $self->_GroupMembersJoin( GroupsAlias => $groups );
        # to avoid joining the table Users into the query, we just join GM
        # and make sure we don't match records where group is member of itself
        $self->Limit(
            LEFTJOIN   => $group_members,
            FIELD      => 'GroupId',
            OPERATOR   => '!=',
            VALUE      => "$group_members.MemberId",
            QUOTEVALUE => 0,
        );
        $self->_SQLLimit(
            ALIAS         => $group_members,
            FIELD         => 'GroupId',
            OPERATOR      => $op,
            VALUE         => $value,
            %rest,
        );
    }
    elsif ( $op =~ /^!=$|^NOT\s+/i ) {
        # negative condition case

        # reverse op
        $op =~ s/!|NOT\s+//i;

        # XXX: we have no way to build correct "Watcher.X != 'Y'" when condition
        # "X = 'Y'" matches more then one user so we try to fetch two records and
        # do the right thing when there is only one exist and semi-working solution
        # otherwise.
        my $users_obj = RT::Users->new( $self->CurrentUser );
        $users_obj->Limit(
            FIELD         => $rest{SUBKEY},
            OPERATOR      => $op,
            VALUE         => $value,
        );
        $users_obj->OrderBy;
        $users_obj->RowsPerPage(2);
        my @users = @{ $users_obj->ItemsArrayRef };

        $group_members ||= $self->_GroupMembersJoin( GroupsAlias => $groups );
        if ( @users <= 1 ) {
            my $uid = 0;
            $uid = $users[0]->id if @users;
            $self->Limit(
                LEFTJOIN      => $group_members,
                ALIAS         => $group_members,
                FIELD         => 'MemberId',
                VALUE         => $uid,
            );
            $self->_SQLLimit(
                %rest,
                ALIAS           => $group_members,
                FIELD           => 'id',
                OPERATOR        => 'IS',
                VALUE           => 'NULL',
            );
        } else {
            $self->Limit(
                LEFTJOIN   => $group_members,
                FIELD      => 'GroupId',
                OPERATOR   => '!=',
                VALUE      => "$group_members.MemberId",
                QUOTEVALUE => 0,
            );
            $users ||= $self->Join(
                TYPE            => 'LEFT',
                ALIAS1          => $group_members,
                FIELD1          => 'MemberId',
                TABLE2          => 'Users',
                FIELD2          => 'id',
            );
            $self->Limit(
                LEFTJOIN      => $users,
                ALIAS         => $users,
                FIELD         => $rest{SUBKEY},
                OPERATOR      => $op,
                VALUE         => $value,
                CASESENSITIVE => 0,
            );
            $self->_SQLLimit(
                %rest,
                ALIAS         => $users,
                FIELD         => 'id',
                OPERATOR      => 'IS',
                VALUE         => 'NULL',
            );
        }
    } else {
        # positive condition case

        $group_members ||= $self->_GroupMembersJoin(
            GroupsAlias => $groups, New => 1, Left => 0
        );
        $users ||= $self->Join(
            TYPE            => 'LEFT',
            ALIAS1          => $group_members,
            FIELD1          => 'MemberId',
            TABLE2          => 'Users',
            FIELD2          => 'id',
        );
        $self->_SQLLimit(
            %rest,
            ALIAS           => $users,
            FIELD           => $rest{'SUBKEY'},
            VALUE           => $value,
            OPERATOR        => $op,
            CASESENSITIVE   => 0,
        );
    }
    $self->_CloseParen;
    return ($groups, $group_members, $users);
}

sub _RoleGroupsJoin {
    my $self = shift;
    my %args = (New => 0, Class => 'Asset', Type => '', @_);
    return $self->{'_sql_role_group_aliases'}{ $args{'Class'} .'-'. $args{'Type'} }
        if $self->{'_sql_role_group_aliases'}{ $args{'Class'} .'-'. $args{'Type'} }
           && !$args{'New'};

    # we always have watcher groups for asset, so we use INNER join
    my $groups = $self->Join(
        ALIAS1          => 'main',
        FIELD1          => $args{'Class'} eq 'Type'? 'Type': 'id',
        TABLE2          => 'Groups',
        FIELD2          => 'Instance',
        ENTRYAGGREGATOR => 'AND',
    );
    $self->Limit(
        LEFTJOIN        => $groups,
        ALIAS           => $groups,
        FIELD           => 'Domain',
        VALUE           => 'RTx::AssetTracker::'. $args{'Class'} .'-Role',
    );
    $self->Limit(
        LEFTJOIN        => $groups,
        ALIAS           => $groups,
        FIELD           => 'Type',
        VALUE           => $args{'Type'},
    ) if $args{'Type'};

    $self->{'_sql_role_group_aliases'}{ $args{'Class'} .'-'. $args{'Type'} } = $groups
        unless $args{'New'};

    return $groups;
}

sub _GroupMembersJoin {
    my $self = shift;
    my %args = (New => 1, GroupsAlias => undef, Left => 1, @_);

    return $self->{'_sql_group_members_aliases'}{ $args{'GroupsAlias'} }
        if $self->{'_sql_group_members_aliases'}{ $args{'GroupsAlias'} }
            && !$args{'New'};

    my $alias = $self->Join(
        $args{'Left'} ? (TYPE            => 'LEFT') : (),
        ALIAS1          => $args{'GroupsAlias'},
        FIELD1          => 'id',
        TABLE2          => 'CachedGroupMembers',
        FIELD2          => 'GroupId',
        ENTRYAGGREGATOR => 'AND',
    );
    $self->Limit(
        $args{'Left'} ? (LEFTJOIN => $alias) : (),
        ALIAS => $alias,
        FIELD => 'Disabled',
        VALUE => 0,
    );

    $self->{'_sql_group_members_aliases'}{ $args{'GroupsAlias'} } = $alias
        unless $args{'New'};

    return $alias;
}

=head2 _WatcherJoin

Helper function which provides joins to a watchers table both for limits
and for ordering.

=cut

sub _WatcherJoin {
    my $self = shift;
    my $type = shift || '';


    my $groups = $self->_RoleGroupsJoin( Type => $type );
    my $group_members = $self->_GroupMembersJoin( GroupsAlias => $groups );
    # XXX: work around, we must hide groups that
    # are members of the role group we search in,
    # otherwise them result in wrong NULLs in Users
    # table and break ordering. Now, we know that
    # RT doesn't allow to add groups as members of the
    # ticket roles, so we just hide entries in CGM table
    # with MemberId == GroupId from results
    $self->Limit(
        LEFTJOIN   => $group_members,
        FIELD      => 'GroupId',
        OPERATOR   => '!=',
        VALUE      => "$group_members.MemberId",
        QUOTEVALUE => 0,
    );
    my $users = $self->Join(
        TYPE            => 'LEFT',
        ALIAS1          => $group_members,
        FIELD1          => 'MemberId',
        TABLE2          => 'Users',
        FIELD2          => 'id',
    );
    return ($groups, $group_members, $users);
}

=head2 _WatcherMembershipLimit

Handle watcher membership limits, i.e. whether the watcher belongs to a
specific group or not.

Meta Data:
  1: Field to query on

SELECT DISTINCT main.*
FROM
    Tickets main,
    Groups Groups_1,
    CachedGroupMembers CachedGroupMembers_2,
    Users Users_3
WHERE (
    (main.EffectiveId = main.id)
) AND (
    (main.Status != 'deleted')
) AND (
    (main.Type = 'ticket')
) AND (
    (
        (Users_3.EmailAddress = '22')
            AND
        (Groups_1.Domain = 'RT::Ticket-Role')
            AND
        (Groups_1.Type = 'RequestorGroup')
    )
) AND
    Groups_1.Instance = main.id
AND
    Groups_1.id = CachedGroupMembers_2.GroupId
AND
    CachedGroupMembers_2.MemberId = Users_3.id
ORDER BY main.id ASC
LIMIT 25

=cut

sub _WatcherMembershipLimit {
    my ( $self, $field, $op, $value, @rest ) = @_;
    my %rest = @rest;

    $self->_OpenParen;

    my $groups       = $self->NewAlias('Groups');
    my $groupmembers = $self->NewAlias('CachedGroupMembers');
    my $users        = $self->NewAlias('Users');
    my $memberships  = $self->NewAlias('CachedGroupMembers');

    if ( ref $field ) {    # gross hack
        my @bundle = @$field;
        $self->_OpenParen;
        for my $chunk (@bundle) {
            ( $field, $op, $value, @rest ) = @$chunk;
            $self->_SQLLimit(
                ALIAS    => $memberships,
                FIELD    => 'GroupId',
                VALUE    => $value,
                OPERATOR => $op,
                @rest,
            );
        }
        $self->_CloseParen;
    }
    else {
        $self->_SQLLimit(
            ALIAS    => $memberships,
            FIELD    => 'GroupId',
            VALUE    => $value,
            OPERATOR => $op,
            @rest,
        );
    }

    # Tie to groups for tickets we care about
    $self->_SQLLimit(
        ALIAS           => $groups,
        FIELD           => 'Domain',
        VALUE           => 'RTx::AssetTracker::Asset-Role',
        ENTRYAGGREGATOR => 'AND'
    );

    $self->Join(
        ALIAS1 => $groups,
        FIELD1 => 'Instance',
        ALIAS2 => 'main',
        FIELD2 => 'id'
    );

    # }}}

    # If we care about which sort of watcher
    my $meta = $FIELD_METADATA{$field};
    my $type = ( defined $meta->[1] ? $meta->[1] : undef );

    if ($type) {
        $self->_SQLLimit(
            ALIAS           => $groups,
            FIELD           => 'Type',
            VALUE           => $type,
            ENTRYAGGREGATOR => 'AND'
        );
    }

    $self->Join(
        ALIAS1 => $groups,
        FIELD1 => 'id',
        ALIAS2 => $groupmembers,
        FIELD2 => 'GroupId'
    );

    $self->Join(
        ALIAS1 => $groupmembers,
        FIELD1 => 'MemberId',
        ALIAS2 => $users,
        FIELD2 => 'id'
    );

    $self->Limit(
        ALIAS => $groupmembers,
        FIELD => 'Disabled',
        VALUE => 0,
    );

    $self->Join(
        ALIAS1 => $memberships,
        FIELD1 => 'MemberId',
        ALIAS2 => $users,
        FIELD2 => 'id'
    );

    $self->Limit(
        ALIAS => $memberships,
        FIELD => 'Disabled',
        VALUE => 0,
    );


    $self->_CloseParen;

}

=head2 _CustomFieldDecipher

Try and turn a CF descriptor into (cfid, cfname) object pair.

Takes an optional second parameter of the CF LookupType, defaults to Asset CFs.

=cut

sub _CustomFieldDecipher {
    my ($self, $string, $lookuptype) = @_;
    $lookuptype ||= $self->_SingularClass->CustomFieldLookupType;

    my ($object, $field, $column) = ($string =~ /^(?:(.+?)\.)?\{(.+)\}(?:\.(Content|LargeContent))?$/);
    $field ||= ($string =~ /^\{(.*?)\}$/)[0] || $string;

    my ($cf, $applied_to);

    if ( $object ) {
        my $record_class = RT::CustomField->RecordClassFromLookupType($lookuptype);
        $applied_to = $record_class->new( $self->CurrentUser );
        $applied_to->Load( $object );

        if ( $applied_to->id ) {
            RT->Logger->debug("Limiting to CFs identified by '$field' applied to $record_class #@{[$applied_to->id]} (loaded via '$object')");
        }
        else {
            RT->Logger->warning("$record_class '$object' doesn't exist, parsed from '$string'");
            $object = 0;
            undef $applied_to;
        }
    }

    if ( $field =~ /\D/ ) {
        $object ||= '';
        my $cfs = RT::CustomFields->new( $self->CurrentUser );
        $cfs->Limit( FIELD => 'Name', VALUE => $field, CASESENSITIVE => 0 );
        $cfs->LimitToLookupType($lookuptype);

        if ($applied_to) {
            $cfs->SetContextObject($applied_to);
            $cfs->LimitToObjectId($applied_to->id);
        }

        # if there is more then one field the current user can
        # see with the same name then we shouldn't return cf object
        # as we don't know which one to use
        $cf = $cfs->First;
        if ( $cf ) {
            $cf = undef if $cfs->Next;
        }
    }
    else {
        $cf = RT::CustomField->new( $self->CurrentUser );
        $cf->Load( $field );
        $cf->SetContextObject($applied_to)
            if $cf->id and $applied_to;
    }

    return ($object, $field, $cf, $column);
}

=head2 _CustomFieldJoin

Factor out the Join of custom fields so we can use it for sorting too

=cut

our %JOIN_ALIAS_FOR_LOOKUP_TYPE = (
    RTx::AssetTracker::Asset->CustomFieldLookupType      => sub { "main" },
);

sub _CustomFieldJoin {
    my ($self, $cfkey, $cfid, $field, $type) = @_;
    $type ||= RTx::AssetTracker::Asset->CustomFieldLookupType;

    # Perform one Join per CustomField
    if ( $self->{_sql_object_cfv_alias}{$cfkey} ||
         $self->{_sql_cf_alias}{$cfkey} )
    {
        return ( $self->{_sql_object_cfv_alias}{$cfkey},
                 $self->{_sql_cf_alias}{$cfkey} );
    }

    my $ObjectAlias = $JOIN_ALIAS_FOR_LOOKUP_TYPE{$type}
        ? $JOIN_ALIAS_FOR_LOOKUP_TYPE{$type}->($self)
        : die "We don't know how to join on $type";

    my ($ObjectCFs, $CFs);
    if ( $cfid ) {
        $ObjectCFs = $self->{_sql_object_cfv_alias}{$cfkey} = $self->Join(
            TYPE   => 'LEFT',
            ALIAS1 => $ObjectAlias,
            FIELD1 => 'id',
            TABLE2 => 'ObjectCustomFieldValues',
            FIELD2 => 'ObjectId',
        );
        $self->Limit(
            LEFTJOIN        => $ObjectCFs,
            FIELD           => 'CustomField',
            VALUE           => $cfid,
            ENTRYAGGREGATOR => 'AND'
        );
    }
    else {
        my $ocfalias = $self->Join(
            TYPE       => 'LEFT',
            FIELD1     => 'Type',
            TABLE2     => 'ObjectCustomFields',
            FIELD2     => 'ObjectId',
        );

        $self->Limit(
            LEFTJOIN        => $ocfalias,
            ENTRYAGGREGATOR => 'OR',
            FIELD           => 'ObjectId',
            VALUE           => '0',
        );

        $CFs = $self->{_sql_cf_alias}{$cfkey} = $self->Join(
            TYPE       => 'LEFT',
            ALIAS1     => $ocfalias,
            FIELD1     => 'CustomField',
            TABLE2     => 'CustomFields',
            FIELD2     => 'id',
        );
        $self->Limit(
            LEFTJOIN        => $CFs,
            ENTRYAGGREGATOR => 'AND',
            FIELD           => 'LookupType',
            VALUE           => $type,
        );
        $self->Limit(
            LEFTJOIN        => $CFs,
            ENTRYAGGREGATOR => 'AND',
            FIELD           => 'Name',
            VALUE           => $field,
        );

        $ObjectCFs = $self->{_sql_object_cfv_alias}{$cfkey} = $self->Join(
            TYPE   => 'LEFT',
            ALIAS1 => $CFs,
            FIELD1 => 'id',
            TABLE2 => 'ObjectCustomFieldValues',
            FIELD2 => 'CustomField',
        );
        $self->Limit(
            LEFTJOIN        => $ObjectCFs,
            FIELD           => 'ObjectId',
            VALUE           => "$ObjectAlias.id",
            QUOTEVALUE      => 0,
            ENTRYAGGREGATOR => 'AND',
        );
    }

    $self->Limit(
        LEFTJOIN        => $ObjectCFs,
        FIELD           => 'ObjectType',
        VALUE           => RT::CustomField->ObjectTypeFromLookupType($type),
        ENTRYAGGREGATOR => 'AND'
    );
    $self->Limit(
        LEFTJOIN        => $ObjectCFs,
        FIELD           => 'Disabled',
        OPERATOR        => '=',
        VALUE           => '0',
        ENTRYAGGREGATOR => 'AND'
    );

    return ($ObjectCFs, $CFs);
}

=head2 _CustomFieldLimit

Limit based on CustomFields

Meta Data:
  none

=cut

use Regexp::Common qw(RE_net_IPv4);
use Regexp::Common::net::CIDR;


sub _CustomFieldLimit {
    my ( $self, $_field, $op, $value, %rest ) = @_;

    my $meta  = $FIELD_METADATA{ $_field };
    my $class = $meta->[1] || 'Asset';
    my $type  = "RTx::AssetTracker::$class"->CustomFieldLookupType;

    my $field = $rest{'SUBKEY'} || die "No field specified";

    # For our sanity, we can only limit on one asset type at a time

    my ($object, $cfid, $cf, $column);
    ($object, $field, $cf, $column) = $self->_CustomFieldDecipher( $field, $type );
    $cfid = $cf ? $cf->id  : 0 ;

# If we're trying to find custom fields that don't match something, we
# want assets where the custom field has no value at all.  Note that
# we explicitly don't include the "IS NULL" case, since we would
# otherwise end up with a redundant clause.

    my ($negative_op, $null_op, $inv_op, $range_op)
        = $self->ClassifySQLOperation( $op );

    my $fix_op = sub {
        return @_ unless RT->Config->Get('DatabaseType') eq 'Oracle';

        my %args = @_;
        return %args unless $args{'FIELD'} eq 'LargeContent';
        
        my $op = $args{'OPERATOR'};
        if ( $op eq '=' ) {
            $args{'OPERATOR'} = 'MATCHES';
        }
        elsif ( $op eq '!=' ) {
            $args{'OPERATOR'} = 'NOT MATCHES';
        }
        elsif ( $op =~ /^[<>]=?$/ ) {
            $args{'FUNCTION'} = "TO_CHAR( $args{'ALIAS'}.LargeContent )";
        }
        return %args;
    };

    if ( $cf && $cf->Type eq 'IPAddress' ) {
        my $parsed = RT::ObjectCustomFieldValue->ParseIP($value);
        if ($parsed) {
            $value = $parsed;
        }
        else {
            $RT::Logger->warn("$value is not a valid IPAddress");
        }
    }

    if ( $cf && $cf->Type eq 'IPAddressRange' ) {

        if ( $value =~ /^\s*$RE{net}{CIDR}{IPv4}{-keep}\s*$/o ) {

            # convert incomplete 192.168/24 to 192.168.0.0/24 format
            $value =
              join( '.', map $_ || 0, ( split /\./, $1 )[ 0 .. 3 ] ) . "/$2"
              || $value;
        }

        my ( $start_ip, $end_ip ) =
          RT::ObjectCustomFieldValue->ParseIPRange($value);
        if ( $start_ip && $end_ip ) {
            if ( $op =~ /^([<>])=?$/ ) {
                my $is_less = $1 eq '<' ? 1 : 0;
                if ( $is_less ) {
                    $value = $start_ip;
                }
                else {
                    $value = $end_ip;
                }
            }
            else {
                $value = join '-', $start_ip, $end_ip;
            }
        }
        else {
            $RT::Logger->warn("$value is not a valid IPAddressRange");
        }
    }

    if ( $cf && $cf->Type =~ /^Date(?:Time)?$/ ) {
        my $date = RT::Date->new( $self->CurrentUser );
        $date->Set( Format => 'unknown', Value => $value );
        if ( $date->Unix ) {

            if (
                   $cf->Type eq 'Date'
                || $value =~ /^\s*(?:today|tomorrow|yesterday)\s*$/i
                || (   $value !~ /midnight|\d+:\d+:\d+/i
                    && $date->Time( Timezone => 'user' ) eq '00:00:00' )
              )
            {
                $value = $date->Date( Timezone => 'user' );
            }
            else {
                $value = $date->DateTime;
            }
        }
        else {
            $RT::Logger->warn("$value is not a valid date string");
        }
    }

    my $single_value = !$cf || !$cfid || $cf->SingleValue;

    my $cfkey = $cfid ? $cfid : "$type-$object.$field";

    if ( $null_op && !$column ) {
        # IS[ NOT] NULL without column is the same as has[ no] any CF value,
        # we can reuse our default joins for this operation
        # with column specified we have different situation
        my ($ObjectCFs, $CFs) = $self->_CustomFieldJoin( $cfkey, $cfid, $field, $type );
        $self->_OpenParen;
        $self->_SQLLimit(
            ALIAS    => $ObjectCFs,
            FIELD    => 'id',
            OPERATOR => $op,
            VALUE    => $value,
            %rest
        );
        $self->_SQLLimit(
            ALIAS      => $CFs,
            FIELD      => 'Name',
            OPERATOR   => 'IS NOT',
            VALUE      => 'NULL',
            QUOTEVALUE => 0,
            ENTRYAGGREGATOR => 'AND',
        ) if $CFs;
        $self->_CloseParen;
    }
    elsif ( $op !~ /^[<>]=?$/ && (  $cf && $cf->Type eq 'IPAddressRange')) {
    
        my ($start_ip, $end_ip) = split /-/, $value;
        
        $self->_OpenParen;
        if ( $op !~ /NOT|!=|<>/i ) { # positive equation
            $self->_CustomFieldLimit(
                $_field, '<=', $end_ip, %rest,
                SUBKEY => $rest{'SUBKEY'}. '.Content',
            );
            $self->_CustomFieldLimit(
                $_field, '>=', $start_ip, %rest,
                SUBKEY          => $rest{'SUBKEY'}. '.LargeContent',
                ENTRYAGGREGATOR => 'AND',
            ); 
            # as well limit borders so DB optimizers can use better
            # estimations and scan less rows
# have to disable this tweak because of ipv6
#            $self->_CustomFieldLimit(
#                $_field, '>=', '000.000.000.000', %rest,
#                SUBKEY          => $rest{'SUBKEY'}. '.Content',
#                ENTRYAGGREGATOR => 'AND',
#            );
#            $self->_CustomFieldLimit(
#                $_field, '<=', '255.255.255.255', %rest,
#                SUBKEY          => $rest{'SUBKEY'}. '.LargeContent',
#                ENTRYAGGREGATOR => 'AND',
#            );  
        }       
        else { # negative equation
            $self->_CustomFieldLimit($_field, '>', $end_ip, %rest);
            $self->_CustomFieldLimit(
                $_field, '<', $start_ip, %rest,
                SUBKEY          => $rest{'SUBKEY'}. '.LargeContent',
                ENTRYAGGREGATOR => 'OR',
            );  
            # TODO: as well limit borders so DB optimizers can use better
            # estimations and scan less rows, but it's harder to do
            # as we have OR aggregator
        }
        $self->_CloseParen;
    } 
    elsif ( !$negative_op || $single_value ) {
        $cfkey .= '.'. $self->{'_sql_multiple_cfs_index'}++ if !$single_value && !$range_op;
        my ($ObjectCFs, $CFs) = $self->_CustomFieldJoin( $cfkey, $cfid, $field, $type );

        $self->_OpenParen;

        $self->_OpenParen;

        $self->_OpenParen;
        # if column is defined then deal only with it
        # otherwise search in Content and in LargeContent
        if ( $column ) {
            $self->_SQLLimit( $fix_op->(
                ALIAS      => $ObjectCFs,
                FIELD      => $column,
                OPERATOR   => $op,
                VALUE      => $value,
                CASESENSITIVE => 0,
                %rest
            ) );
            $self->_CloseParen;
            $self->_CloseParen;
            $self->_CloseParen;
        }
        else {
            # need special treatment for Date
            if ( $cf and $cf->Type eq 'DateTime' and $op eq '=' && $value !~ /:/ ) {
                # no time specified, that means we want everything on a
                # particular day.  in the database, we need to check for >
                # and < the edges of that day.
                    my $date = RT::Date->new( $self->CurrentUser );
                    $date->Set( Format => 'unknown', Value => $value );
                    my $daystart = $date->ISO;
                    $date->AddDay;
                    my $dayend = $date->ISO;

                    $self->_OpenParen;

                    $self->_SQLLimit(
                        ALIAS    => $ObjectCFs,
                        FIELD    => 'Content',
                        OPERATOR => ">=",
                        VALUE    => $daystart,
                        %rest,
                    );

                    $self->_SQLLimit(
                        ALIAS    => $ObjectCFs,
                        FIELD    => 'Content',
                        OPERATOR => "<",
                        VALUE    => $dayend,
                        %rest,
                        ENTRYAGGREGATOR => 'AND',
                    );

                    $self->_CloseParen;
            }
            elsif ( $op eq '=' || $op eq '!=' || $op eq '<>' ) {
                if ( length( Encode::encode_utf8($value) ) < 256 ) {
                    $self->_SQLLimit(
                        ALIAS    => $ObjectCFs,
                        FIELD    => 'Content',
                        OPERATOR => $op,
                        VALUE    => $value,
                        CASESENSITIVE => 0,
                        %rest
                    );
                }
                else {
                    $self->_OpenParen;
                    $self->_SQLLimit(
                        ALIAS           => $ObjectCFs,
                        FIELD           => 'Content',
                        OPERATOR        => '=',
                        VALUE           => '',
                        ENTRYAGGREGATOR => 'OR'
                    );
                    $self->_SQLLimit(
                        ALIAS           => $ObjectCFs,
                        FIELD           => 'Content',
                        OPERATOR        => 'IS',
                        VALUE           => 'NULL',
                        ENTRYAGGREGATOR => 'OR'
                    );
                    $self->_CloseParen;
                    $self->_SQLLimit( $fix_op->(
                        ALIAS           => $ObjectCFs,
                        FIELD           => 'LargeContent',
                        OPERATOR        => $op,
                        VALUE           => $value,
                        ENTRYAGGREGATOR => 'AND',
                        CASESENSITIVE => 0,
                    ) );
                }
            }
            else {
                $self->_SQLLimit(
                    ALIAS    => $ObjectCFs,
                    FIELD    => 'Content',
                    OPERATOR => $op,
                    VALUE    => $value,
                    CASESENSITIVE => 0,
                    %rest
                );

                $self->_OpenParen;
                $self->_OpenParen;
                $self->_SQLLimit(
                    ALIAS           => $ObjectCFs,
                    FIELD           => 'Content',
                    OPERATOR        => '=',
                    VALUE           => '',
                    ENTRYAGGREGATOR => 'OR'
                );
                $self->_SQLLimit(
                    ALIAS           => $ObjectCFs,
                    FIELD           => 'Content',
                    OPERATOR        => 'IS',
                    VALUE           => 'NULL',
                    ENTRYAGGREGATOR => 'OR'
                );
                $self->_CloseParen;
                $self->_SQLLimit( $fix_op->(
                    ALIAS           => $ObjectCFs,
                    FIELD           => 'LargeContent',
                    OPERATOR        => $op,
                    VALUE           => $value,
                    ENTRYAGGREGATOR => 'AND',
                    CASESENSITIVE => 0,
                ) );
                $self->_CloseParen;
            }
            $self->_CloseParen;

            # XXX: if we join via CustomFields table then
            # because of order of left joins we get NULLs in
            # CF table and then get nulls for those records
            # in OCFVs table what result in wrong results
            # as decifer method now tries to load a CF then
            # we fall into this situation only when there
            # are more than one CF with the name in the DB.
            # the same thing applies to order by call.
            # TODO: reorder joins T <- OCFVs <- CFs <- OCFs if
            # we want treat IS NULL as (not applies or has
            # no value)
            $self->_SQLLimit(
                ALIAS           => $CFs,
                FIELD           => 'Name',
                OPERATOR        => 'IS NOT',
                VALUE           => 'NULL',
                QUOTEVALUE      => 0,
                ENTRYAGGREGATOR => 'AND',
            ) if $CFs;
            $self->_CloseParen;

            if ($negative_op) {
                $self->_SQLLimit(
                    ALIAS           => $ObjectCFs,
                    FIELD           => $column || 'Content',
                    OPERATOR        => 'IS',
                    VALUE           => 'NULL',
                    QUOTEVALUE      => 0,
                    ENTRYAGGREGATOR => 'OR',
                );
            }

            $self->_CloseParen;
        }
    }
    else {
        $cfkey .= '.'. $self->{'_sql_multiple_cfs_index'}++;
        my ($ObjectCFs, $CFs) = $self->_CustomFieldJoin( $cfkey, $cfid, $field, $type );

        # reverse operation
        $op =~ s/!|NOT\s+//i;

        # if column is defined then deal only with it
        # otherwise search in Content and in LargeContent
        if ( $column ) {
            $self->Limit( $fix_op->(
                LEFTJOIN   => $ObjectCFs,
                ALIAS      => $ObjectCFs,
                FIELD      => $column,
                OPERATOR   => $op,
                VALUE      => $value,
                CASESENSITIVE => 0,
            ) );
        }
        else {
            $self->Limit(
                LEFTJOIN   => $ObjectCFs,
                ALIAS      => $ObjectCFs,
                FIELD      => 'Content',
                OPERATOR   => $op,
                VALUE      => $value,
                CASESENSITIVE => 0,
            );
        }
        $self->_SQLLimit(
            %rest,
            ALIAS      => $ObjectCFs,
            FIELD      => 'id',
            OPERATOR   => 'IS',
            VALUE      => 'NULL',
            QUOTEVALUE => 0,
        );
    }
}


# End Helper Functions

# End of SQL Stuff -------------------------------------------------


=head2 OrderByCols ARRAY

A modified version of the OrderBy method which automatically joins where
C<ALIAS> is set to the name of a watcher type.

=cut

sub OrderByCols {
    my $self = shift;
    my @args = @_;
    my $clause;
    my @res   = ();
    my $order = 0;

    foreach my $row (@args) {
        if ( $row->{ALIAS} ) {
            push @res, $row;
            next;
        }
        if ( $row->{FIELD} !~ /\./ ) {
            my $meta = $self->FIELDS->{ $row->{FIELD} };
            unless ( $meta ) {
                push @res, $row;
                next;
            }

            if ( $meta->[0] eq 'ENUM' && ($meta->[1]||'') eq 'Type' ) {
                my $alias = $self->Join(
                    TYPE   => 'LEFT',
                    ALIAS1 => 'main',
                    FIELD1 => $row->{'FIELD'},
                    TABLE2 => 'AT_Types',
                    FIELD2 => 'id',
                );
                push @res, { %$row, ALIAS => $alias, FIELD => "Name" };
            } elsif ( ( $meta->[0] eq 'ENUM' && ($meta->[1]||'') eq 'User' )
                || ( $meta->[0] eq 'WATCHERFIELD' && ($meta->[1]||'') eq 'Owner' )
            ) {
                my $alias = $self->Join(
                    TYPE   => 'LEFT',
                    ALIAS1 => 'main',
                    FIELD1 => $row->{'FIELD'},
                    TABLE2 => 'Users',
                    FIELD2 => 'id',
                );
                push @res, { %$row, ALIAS => $alias, FIELD => "Name" };
            } else {
                push @res, $row;
            }
            next;
        }

        my ( $field, $subkey ) = split /\./, $row->{FIELD}, 2;
        my $meta = $self->FIELDS->{$field};
        if ( defined $meta->[0] && $meta->[0] eq 'WATCHERFIELD' ) {
            # cache alias as we want to use one alias per watcher type for sorting
            my $users = $self->{_sql_u_watchers_alias_for_sort}{ $meta->[1] };
            unless ( $users ) {
                $self->{_sql_u_watchers_alias_for_sort}{ $meta->[1] }
                    = $users = ( $self->_WatcherJoin( $meta->[1] ) )[2];
            }
            push @res, { %$row, ALIAS => $users, FIELD => $subkey };
       } elsif ( defined $meta->[0] && $meta->[0] eq 'CUSTOMFIELD' ) {
           my ($object, $field, $cf_obj, $column) = $self->_CustomFieldDecipher( $subkey );
           my $cfkey = $cf_obj ? $cf_obj->id : "$object.$field";
           $cfkey .= ".ordering" if !$cf_obj || ($cf_obj->MaxValues||0) != 1;
           my ($ObjectCFs, $CFs) = $self->_CustomFieldJoin( $cfkey, ($cf_obj ?$cf_obj->id :0) , $field );
           # this is described in _CustomFieldLimit
           $self->_SQLLimit(
               ALIAS      => $CFs,
               FIELD      => 'Name',
               OPERATOR   => 'IS NOT',
               VALUE      => 'NULL',
               QUOTEVALUE => 1,
               ENTRYAGGREGATOR => 'AND',
           ) if $CFs;
           unless ($cf_obj) {
               # For those cases where we are doing a join against the
               # CF name, and don't have a CFid, use Unique to make sure
               # we don't show duplicate assets.  NOTE: I'm pretty sure
               # this will stay mixed in for the life of the
               # class/package, and not just for the life of the object.
               # Potential performance issue.
               require DBIx::SearchBuilder::Unique;
               DBIx::SearchBuilder::Unique->import;
           }
           my $CFvs = $self->Join(
               TYPE   => 'LEFT',
               ALIAS1 => $ObjectCFs,
               FIELD1 => 'CustomField',
               TABLE2 => 'CustomFieldValues',
               FIELD2 => 'CustomField',
           );
           $self->Limit(
               LEFTJOIN        => $CFvs,
               FIELD           => 'Name',
               QUOTEVALUE      => 0,
               VALUE           => $ObjectCFs . ".Content",
               ENTRYAGGREGATOR => 'AND'
           );

           push @res, { %$row, ALIAS => $CFvs, FIELD => 'SortOrder' };
           push @res, { %$row, ALIAS => $ObjectCFs, FIELD => 'Content' };
       }
       else {
           push @res, $row;
       }
    }
    return $self->SUPER::OrderByCols(@res);
}


sub Limit {
    my $self = shift;
    my %args = @_;
    $self->{'must_redo_search'} = 1;
    delete $self->{'raw_rows'};
    delete $self->{'count_all'};

    $args{ALIAS} ||= 'main';

    if ($self->{'using_restrictions'}) {
        RT->Deprecated( Message => "Mixing old-style LimitFoo methods with Limit is deprecated" );
        $self->LimitField(@_);
    }
    $self->SUPER::Limit(@_);
}


=head2 LimitField

Takes a paramhash with the fields FIELD, OPERATOR, VALUE and DESCRIPTION
Generally best called from LimitFoo methods

=cut

sub LimitField {
    my $self = shift;
    my %args = (
        FIELD       => undef,
        OPERATOR    => '=',
        VALUE       => undef,
        DESCRIPTION => undef,
        @_
    );
    $args{'DESCRIPTION'} = $self->loc(
        "[_1] [_2] [_3]",  $args{'FIELD'},
        $args{'OPERATOR'}, $args{'VALUE'}
        )
        if ( !defined $args{'DESCRIPTION'} );


    if ($self->_isLimited > 1) {
        RT->Deprecated( Message => "Mixing old-style LimitFoo methods with Limit is deprecated" );
    }
    $self->{using_restrictions} = 1;

    my $index = $self->_NextIndex;

# make the AssetRestrictions hash the equivalent of whatever we just passed in;

    %{ $self->{'AssetRestrictions'}{$index} } = %args;

    $self->{'RecalcAssetLimits'} = 1;

    return ($index);
}




=head2 LimitType

LimitType takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of = or !=. (It defaults to =).
VALUE is an asset type id or Name.


=cut

sub LimitType {
    my $self = shift;
    my %args = (
        VALUE    => undef,
        OPERATOR => '=',
        @_
    );

    #TODO  VALUE should also take queue objects
    if ( defined $args{'VALUE'} && $args{'VALUE'} !~ /^\d+$/ ) {
        my $type = RTx::AssetTracker::Type->new( $self->CurrentUser );
        $type->Load( $args{'VALUE'} );
        $args{'VALUE'} = $type->Id;
    }

    # What if they pass in an Id?  Check for isNum() and convert to
    # string.

    #TODO check for a valid queue here

    $self->LimitField(
        FIELD       => 'Type',
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        DESCRIPTION => join(
            ' ', $self->loc('Type'), $args{'OPERATOR'}, $args{'VALUE'},
        ),
    );

}



=head2 LimitStatus

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of = or !=.
VALUE is a status.

RT adds Status != 'deleted' until object has
allow_deleted_search internal property set.
$assets->{'allow_deleted_search'} = 1;
$assets->LimitStatus( VALUE => 'deleted' );

=cut

sub LimitStatus {
    my $self = shift;
    my %args = (
        OPERATOR => '=',
        @_
    );
    $self->LimitField(
        FIELD       => 'Status',
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        DESCRIPTION => join( ' ',
            $self->loc('Status'), $args{'OPERATOR'},
            $self->loc( $args{'VALUE'} ) ),
    );
}



=head2 IgnoreType

Stub. AT doesn't use this flag because assets don't have an equivalent
to "ticket types")

=cut

sub IgnoreType {
    my $self = shift;

    # Instead of faking a Limit that later gets ignored, fake up the
    # fact that we're already looking at type, so that the check in
    # Tickets_SQL/FromSQL goes down the right branch

    #  $self->LimitType(VALUE => '__any');
    #$self->{looking_at_type} = 1;
}



=head2 LimitDescription

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of = or !=.
VALUE is a string to search for in the description of the asset.

=cut

sub LimitDescription {
    my $self = shift;
    my %args = (@_);
    $self->LimitField(
        FIELD       => 'Description',
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        DESCRIPTION => join( ' ',
            $self->loc('Description'), $args{'OPERATOR'}, $args{'VALUE'},
        ),
    );
}



=head2 LimitId

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of =, >, < or !=.
VALUE is an asset Id to search for

=cut

sub LimitId {
    my $self = shift;
    my %args = (
        OPERATOR => '=',
        @_
    );

    $self->LimitField(
        FIELD       => 'id',
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        DESCRIPTION =>
            join( ' ', $self->loc('Id'), $args{'OPERATOR'}, $args{'VALUE'}, ),
    );
}



=head2 LimitURI

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of =, >, < or !=.
VALUE is an asset URI to search for

=cut

sub LimitURI {
    my $self = shift;
    my %args = (
        OPERATOR => '=',
        @_
    );

    $self->LimitField(
        FIELD => 'URI',
        VALUE => $args{'VALUE'},
        OPERATOR => $args{'OPERATOR'},
        DESCRIPTION => join( ' ',
            $self->loc('Id'), $args{'OPERATOR'}, $args{'VALUE'},
        ),
    );
}




=head2 LimitWatcher

  Takes a paramhash with the fields OPERATOR, TYPE and VALUE.
  OPERATOR is one of =, LIKE, NOT LIKE or !=.
  VALUE is a value to match the ticket\'s watcher email addresses against
  TYPE is the sort of watchers you want to match against. Leave it undef if you want to search all of them

=cut

sub LimitWatcher {
    my $self = shift;
    my %args = (
        OPERATOR => '=',
        VALUE    => undef,
        TYPE     => undef,
        @_
    );

    #build us up a description
    my ( $watcher_type, $desc );
    if ( $args{'TYPE'} ) {
        $watcher_type = $args{'TYPE'};
    }
    else {
        $watcher_type = "Watcher";
    }

    $self->LimitField(
        FIELD       => $watcher_type,
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        TYPE        => $args{'TYPE'},
        DESCRIPTION => join( ' ',
            $self->loc($watcher_type),
            $args{'OPERATOR'}, $args{'VALUE'}, ),
    );
}






=head2 LimitLinkedTo

LimitLinkedTo takes a paramhash with two fields: TYPE and TARGET
TYPE limits the sort of link we want to search on

TYPE = one of Asset->RoleGroupTypes

TARGET is the id or URI of the TARGET of the link

=cut

sub LimitLinkedTo {
    my $self = shift;
    my %args = (
        TARGET   => undef,
        TYPE     => undef,
        OPERATOR => '=',
        @_
    );

    $self->LimitField(
        FIELD       => 'LinkedTo',
        BASE        => undef,
        TARGET      => $args{'TARGET'},
        TYPE        => $args{'TYPE'},
        DESCRIPTION => $self->loc(
            "Assets [_1] by [_2]",
            $self->loc( $args{'TYPE'} ),
            $args{'TARGET'}
        ),
        OPERATOR    => $args{'OPERATOR'},
    );
}



=head2 LimitLinkedFrom

LimitLinkedFrom takes a paramhash with two fields: TYPE and BASE
TYPE limits the sort of link we want to search on


BASE is the id or URI of the BASE of the link

=cut

sub LimitLinkedFrom {
    my $self = shift;
    my %args = (
        BASE     => undef,
        TYPE     => undef,
        OPERATOR => '=',
        @_
    );

    $self->LimitField(
        FIELD       => 'LinkedTo',
        TARGET      => undef,
        BASE        => $args{'BASE'},
        TYPE        => $args{'TYPE'},
        DESCRIPTION => $self->loc(
            "Assets [_1] [_2]",
            $self->loc( $args{'TYPE'} ),
            $args{'BASE'}
        ),
        OPERATOR    => $args{'OPERATOR'},
    );
}




=head2 LimitDate (FIELD => 'DateField', OPERATOR => $oper, VALUE => $ISODate)

Takes a paramhash with the fields FIELD OPERATOR and VALUE.

OPERATOR is one of > or <
VALUE is a date and time in ISO format in GMT
FIELD is one of Created, LastUpdated

There are also helper functions of the form LimitFIELD that eliminate
the need to pass in a FIELD argument.

=cut

sub LimitDate {
    my $self = shift;
    my %args = (
        FIELD    => undef,
        VALUE    => undef,
        OPERATOR => undef,

        @_
    );

    #Set the description if we didn't get handed it above
    unless ( $args{'DESCRIPTION'} ) {
        $args{'DESCRIPTION'} = $args{'FIELD'} . " "
            . $args{'OPERATOR'} . " "
            . $args{'VALUE'} . " GMT";
    }

    $self->LimitField(%args);

}


sub LimitCreated {
    my $self = shift;
    $self->LimitDate( FIELD => 'Created', @_ );
}
sub LimitLastUpdated {
    my $self = shift;
    $self->LimitDate( FIELD => 'LastUpdated', @_ );
}

#

=head2 LimitTransactionDate (OPERATOR => $oper, VALUE => $ISODate)

Takes a paramhash with the fields FIELD OPERATOR and VALUE.

OPERATOR is one of > or <
VALUE is a date and time in ISO format in GMT


=cut

sub LimitTransactionDate {
    my $self = shift;
    my %args = (
        FIELD    => 'TransactionDate',
        VALUE    => undef,
        OPERATOR => undef,

        @_
    );

    #  <20021217042756.GK28744@pallas.fsck.com>
    #    "Kill It" - Jesse.

    #Set the description if we didn't get handed it above
    unless ( $args{'DESCRIPTION'} ) {
        $args{'DESCRIPTION'} = $args{'FIELD'} . " "
            . $args{'OPERATOR'} . " "
            . $args{'VALUE'} . " GMT";
    }

    $self->LimitField(%args);

}




=head2 LimitCustomField

Takes a paramhash of key/value pairs with the following keys:

=over 4

=item CUSTOMFIELD - CustomField name or id.  If a name is passed, an additional parameter ASSETTYPE may also be passed to distinguish the custom field.

=item OPERATOR - The usual Limit operators

=item VALUE - The value to compare against

=back

=cut

sub LimitCustomField {
    my $self = shift;
    my %args = (
        VALUE       => undef,
        CUSTOMFIELD => undef,
        OPERATOR    => '=',
        DESCRIPTION => undef,
        FIELD       => 'CustomFieldValue',
        QUOTEVALUE  => 1,
        @_
    );

    my $CF = RT::CustomField->new( $self->CurrentUser );
    if ( $args{CUSTOMFIELD} =~ /^\d+$/ ) {
        $CF->Load( $args{CUSTOMFIELD} );
    }
    else {
        $CF->LoadByNameAndAssetType(
            Name      => $args{CUSTOMFIELD},
            AssetType => $args{ASSETTYPE}
        );
        $args{CUSTOMFIELD} = $CF->Id;
    }

    #If we are looking to compare with a null value.
    if ( $args{'OPERATOR'} =~ /^is$/i ) {
        $args{'DESCRIPTION'}
            ||= $self->loc( "Custom field [_1] has no value.", $CF->Name );
    }
    elsif ( $args{'OPERATOR'} =~ /^is not$/i ) {
        $args{'DESCRIPTION'}
            ||= $self->loc( "Custom field [_1] has a value.", $CF->Name );
    }

    # if we're not looking to compare with a null value
    else {
        $args{'DESCRIPTION'} ||= $self->loc( "Custom field [_1] [_2] [_3]",
            $CF->Name, $args{OPERATOR}, $args{VALUE} );
    }

    if ( defined $args{'ASSETTYPE'} && $args{'ASSETTYPE'} =~ /\D/ ) {
        my $TypeObj = RTx::AssetTracker::Type->new( $self->CurrentUser );
        $TypeObj->Load( $args{'ASSETTYPE'} );
        $args{'ASSETTYPE'} = $TypeObj->Id;
    }
    delete $args{'ASSETTYPE'} unless defined $args{'ASSETTYPE'} && length $args{'ASSETTYPE'};

    my @rest;
    @rest = ( ENTRYAGGREGATOR => 'AND' )
        if ( $CF->Type eq 'SelectMultiple' );

    $self->LimitField(
        VALUE => $args{VALUE},
        FIELD => "CF"
            .(defined $args{'ASSETTYPE'}? ".$args{'ASSETTYPE'}" : '' )
            .".{" . $CF->Name . "}",
        OPERATOR    => $args{OPERATOR},
        CUSTOMFIELD => 1,
        @rest,
    );

    $self->{'RecalcAssetLimits'} = 1;
}



=head2 _NextIndex

Keep track of the counter for the array of restrictions

=cut

sub _NextIndex {
    my $self = shift;
    return ( $self->{'restriction_index'}++ );
}
 



sub _Init {
    my $self = shift;
    $self->{'table'}                   = "AT_Assets";
    $self->{'RecalcAssetLimits'}       = 1;
    $self->{'restriction_index'}       = 1;
    $self->{'primary_key'}             = "id";
    delete $self->{'items_array'};
    delete $self->{'item_map'};
    delete $self->{'columns_to_display'};
    $self->SUPER::_Init(@_);

    $self->_InitSQL;

}


sub Count {
    my $self = shift;
    $self->_ProcessRestrictions() if ( $self->{'RecalcAssetLimits'} == 1 );
    return ( $self->SUPER::Count() );
}


sub CountAll {
    my $self = shift;
    $self->_ProcessRestrictions() if ( $self->{'RecalcAssetLimits'} == 1 );
    return ( $self->SUPER::CountAll() );
}



=head2 ItemsArrayRef

Returns a reference to the set of all items found in this search

=cut

sub ItemsArrayRef {
    my $self = shift;

    return $self->{'items_array'} if $self->{'items_array'};

    my $placeholder = $self->_ItemsCounter;
    $self->GotoFirstItem();
    while ( my $item = $self->Next ) {
        push( @{ $self->{'items_array'} }, $item );
    }
    $self->GotoItem($placeholder);
    $self->{'items_array'}
        = $self->ItemsOrderBy( $self->{'items_array'} );

    return $self->{'items_array'};
}

sub ItemsArrayRefWindow {
    my $self = shift;
    my $window = shift;

    my @old = ($self->_ItemsCounter, $self->RowsPerPage, $self->FirstRow+1);

    $self->RowsPerPage( $window );
    $self->FirstRow(1);
    $self->GotoFirstItem;

    my @res;
    while ( my $item = $self->Next ) {
        push @res, $item;
    }

    $self->RowsPerPage( $old[1] );
    $self->FirstRow( $old[2] );
    $self->GotoItem( $old[0] );

    return \@res;
}


sub Next {
    my $self = shift;

    $self->_ProcessRestrictions() if ( $self->{'RecalcAssetLimits'} == 1 );

    my $Asset = $self->SUPER::Next;
    return $Asset unless $Asset;

    if ( $Asset->__Value('Status') eq 'deleted'
        && !$self->{'allow_deleted_search'} )
    {
        return $self->Next;
    }
    elsif ( RT->Config->Get('UseSQLForACLChecks') ) {
        # if we found an asset with this option enabled then
        # all assets we found are ACLed, cache this fact
        my $key = join ";:;", $self->CurrentUser->id, 'ShowAsset', 'RTx::AssetTracker::Asset-'. $Asset->id;
        $RT::Principal::_ACL_CACHE->{ $key } = 1;
        return $Asset;
    }
    elsif ( $Asset->CurrentUserHasRight('ShowAsset') ) {
        # has rights
        return $Asset;
    }
    else {	
        # If the user doesn't have the right to show this ticket
        return $self->Next;
    }
}


sub _DoSearch {
    my $self = shift;
    $self->CurrentUserCanSee if RT->Config->Get('UseSQLForACLChecks');
    return $self->SUPER::_DoSearch( @_ );
}

sub _DoCount {
    my $self = shift;
    $self->CurrentUserCanSee if RT->Config->Get('UseSQLForACLChecks');
    return $self->SUPER::_DoCount( @_ );
}

sub _RolesCanSee {
    my $self = shift;

    my $cache_key = 'RolesHasRight;:;ShowAsset';
 
    if ( my $cached = $RT::Principal::_ACL_CACHE->{ $cache_key } ) {
        return %$cached;
    }

    my $ACL = RT::ACL->new( RT->SystemUser );
    $ACL->Limit( FIELD => 'RightName', VALUE => 'ShowAsset' );
    $ACL->Limit( FIELD => 'PrincipalType', OPERATOR => '!=', VALUE => 'Group' );
    my $principal_alias = $ACL->Join(
        ALIAS1 => 'main',
        FIELD1 => 'PrincipalId',
        TABLE2 => 'Principals',
        FIELD2 => 'id',
    );
    $ACL->Limit( ALIAS => $principal_alias, FIELD => 'Disabled', VALUE => 0 );

    my %res = ();
    foreach my $ACE ( @{ $ACL->ItemsArrayRef } ) {
        my $role = $ACE->__Value('PrincipalType');
        my $type = $ACE->__Value('ObjectType');
        if ( $type eq 'RT::System' ) {
            $res{ $role } = 1;
        }
        elsif ( $type eq 'RTx::AssetTracker::Type' ) {
            next if $res{ $role } && !ref $res{ $role };
            push @{ $res{ $role } ||= [] }, $ACE->__Value('ObjectId');
        }
        else {
            $RT::Logger->error('ShowAsset right is granted on unsupported object');
        }
    }
    $RT::Principal::_ACL_CACHE->{ $cache_key } = \%res;
    return %res;
}

sub _DirectlyCanSeeIn {
    my $self = shift;
    my $id = $self->CurrentUser->id;

    my $cache_key = 'User-'. $id .';:;ShowAsset;:;DirectlyCanSeeIn';
    if ( my $cached = $RT::Principal::_ACL_CACHE->{ $cache_key } ) {
        return @$cached;
    }

    my $ACL = RT::ACL->new( RT->SystemUser );
    $ACL->Limit( FIELD => 'RightName', VALUE => 'ShowAsset' );
    my $principal_alias = $ACL->Join(
        ALIAS1 => 'main',
        FIELD1 => 'PrincipalId',
        TABLE2 => 'Principals',
        FIELD2 => 'id',
    );
    $ACL->Limit( ALIAS => $principal_alias, FIELD => 'Disabled', VALUE => 0 );
    my $cgm_alias = $ACL->Join(
        ALIAS1 => 'main',
        FIELD1 => 'PrincipalId',
        TABLE2 => 'CachedGroupMembers',
        FIELD2 => 'GroupId',
    );
    $ACL->Limit( ALIAS => $cgm_alias, FIELD => 'MemberId', VALUE => $id );
    $ACL->Limit( ALIAS => $cgm_alias, FIELD => 'Disabled', VALUE => 0 );

    my @res = ();
    foreach my $ACE ( @{ $ACL->ItemsArrayRef } ) {
        my $type = $ACE->__Value('ObjectType');
        if ( $type eq 'RT::System' ) {
            # If user is direct member of a group that has the right
            # on the system then he can see any ticket
            $RT::Principal::_ACL_CACHE->{ $cache_key } = [-1];
            return (-1);
        }
        elsif ( $type eq 'RTx::AssetTracker::Type' ) {
            push @res, $ACE->__Value('ObjectId');
        }
        else {
            $RT::Logger->error('ShowAsset right is granted on unsupported object');
        }
    }
    $RT::Principal::_ACL_CACHE->{ $cache_key } = \@res;
    return @res;
}

sub CurrentUserCanSee {
    my $self = shift;
    return if $self->{'_sql_current_user_can_see_applied'};

    return $self->{'_sql_current_user_can_see_applied'} = 1
        if $self->CurrentUser->UserObj->HasRight(
            Right => 'SuperUser', Object => $RT::System
        );

    local $self->{using_restrictions};

    my $id = $self->CurrentUser->id;

    # directly can see in all asset types then we have nothing to do
    my @direct_types = $self->_DirectlyCanSeeIn;
    return $self->{'_sql_current_user_can_see_applied'} = 1
        if @direct_types && $direct_types[0] == -1;

    my %roles = $self->_RolesCanSee;
    {
        my %skip = map { $_ => 1 } @direct_types;
        foreach my $role ( keys %roles ) {
            next unless ref $roles{ $role };

            my @types = grep !$skip{$_}, @{ $roles{ $role } };
            if ( @types ) {
                $roles{ $role } = \@types;
            } else {
                delete $roles{ $role };
            }
        }
    }

# there is no global watchers, only asset types and assets, if at
# some point we will add global roles then it's gonna blow
# the idea here is that if the right is set globally for a role
# and user plays this role for a type directly not an asset
# then we have to check in advance
    if ( my @tmp = grep !ref $roles{ $_ }, keys %roles ) {

        my $groups = RT::Groups->new( RT->SystemUser );
        $groups->Limit( FIELD => 'Domain', VALUE => 'RTx::AssetTracker::Type-Role' );
        foreach ( @tmp ) {
            $groups->Limit( FIELD => 'Name', VALUE => $_ );
        }
        my $principal_alias = $groups->Join(
            ALIAS1 => 'main',
            FIELD1 => 'id',
            TABLE2 => 'Principals',
            FIELD2 => 'id',
        );
        $groups->Limit( ALIAS => $principal_alias, FIELD => 'Disabled', VALUE => 0 );
        my $cgm_alias = $groups->Join(
            ALIAS1 => 'main',
            FIELD1 => 'id',
            TABLE2 => 'CachedGroupMembers',
            FIELD2 => 'GroupId',
        );
        $groups->Limit( ALIAS => $cgm_alias, FIELD => 'MemberId', VALUE => $id );
        $groups->Limit( ALIAS => $cgm_alias, FIELD => 'Disabled', VALUE => 0 );
        while ( my $group = $groups->Next ) {
            push @direct_types, $group->Instance;
        }
    }

    unless ( @direct_types || keys %roles ) {
        $self->Limit(
            SUBCLAUSE => 'ACL',
            ALIAS => 'main',
            FIELD => 'id',
            VALUE => 0,
            ENTRYAGGREGATOR => 'AND',
        );
        return $self->{'_sql_current_user_can_see_applied'} = 1;
    }

    {
        my $join_roles = keys %roles;
        my ($role_group_alias, $cgm_alias);
        if ( $join_roles ) {
            $role_group_alias = $self->_RoleGroupsJoin( New => 1 );
            $cgm_alias = $self->_GroupMembersJoin( GroupsAlias => $role_group_alias );
            $self->Limit(
                LEFTJOIN   => $cgm_alias,
                FIELD      => 'MemberId',
                OPERATOR   => '=',
                VALUE      => $id,
            );
        }
        my $limit_types = sub {
            my $ea = shift;
            my @types = @_;

            return unless @types;
            if ( @types == 1 ) {
                $self->Limit(
                    SUBCLAUSE => 'ACL',
                    ALIAS => 'main',
                    FIELD => 'Name',
                    VALUE => $_[0],
                    ENTRYAGGREGATOR => $ea,
                );
            } else {
                $self->SUPER::_OpenParen('ACL');
                foreach my $q ( @types ) {
                    $self->Limit(
                        SUBCLAUSE => 'ACL',
                        ALIAS => 'main',
                        FIELD => 'Name',
                        VALUE => $q,
                        ENTRYAGGREGATOR => $ea,
                    );
                    $ea = 'OR';
                }
                $self->SUPER::_CloseParen('ACL');
            }
            return 1;
        };

        $self->SUPER::_OpenParen('ACL');
        my $ea = 'AND';
        $ea = 'OR' if $limit_types->( $ea, @direct_types );
        while ( my ($role, $types) = each %roles ) {
            $self->SUPER::_OpenParen('ACL');
            $self->Limit(
                SUBCLAUSE       => 'ACL',
                ALIAS           => $cgm_alias,
                FIELD           => 'MemberId',
                OPERATOR        => 'IS NOT',
                VALUE           => 'NULL',
                QUOTEVALUE      => 0,
                ENTRYAGGREGATOR => $ea,
            );
            $self->Limit(
                SUBCLAUSE       => 'ACL',
                ALIAS           => $role_group_alias,
                FIELD           => 'Name',
                VALUE           => $role,
                ENTRYAGGREGATOR => 'AND',
            );
            $limit_types->( 'AND', @$types ) if ref $types;
            $ea = 'OR' if $ea eq 'AND';
            $self->SUPER::_CloseParen('ACL');
        }
        $self->SUPER::_CloseParen('ACL');
    }
    return $self->{'_sql_current_user_can_see_applied'} = 1;
}





=head2 LoadRestrictions

LoadRestrictions takes a string which can fully populate the AssetRestrictons hash.
TODO It is not yet implemented

=cut



=head2 DescribeRestrictions

takes nothing.
Returns a hash keyed by restriction id.
Each element of the hash is currently a one element hash that contains DESCRIPTION which
is a description of the purpose of that AssetRestriction

=cut

sub DescribeRestrictions {
    my $self = shift;

    my %listing;

    foreach my $row ( keys %{ $self->{'AssetRestrictions'} } ) {
	$listing{$row} = $self->{'AssetRestrictions'}{$row}{'DESCRIPTION'};
    }
    return (%listing);
}



=head2 RestrictionValues FIELD

Takes a restriction field and returns a list of values this field is restricted
to.

=cut

sub RestrictionValues {
    my $self  = shift;
    my $field = shift;
    map $self->{'AssetRestrictions' }{$_}{'VALUE'}, grep {
               $self->{'AssetRestrictions'}{$_}{'FIELD'}    eq $field
            && $self->{'AssetRestrictions'}{$_}{'OPERATOR'} eq "="
        }
        keys %{ $self->{'AssetRestrictions'} };
}



=head2 ClearRestrictions

Removes all restrictions irretrievably

=cut

sub ClearRestrictions {
    my $self = shift;
    delete $self->{'AssetRestrictions'};
    $self->{'RecalcAssetLimits'} = 1;
}



=head2 DeleteRestriction

Takes the row Id of a restriction (From DescribeRestrictions' output, for example.
Removes that restriction from the session's limits.

=cut

sub DeleteRestriction {
    my $self = shift;
    my $row  = shift;
    delete $self->{'AssetRestrictions'}{$row};

    $self->{'RecalcAssetLimits'} = 1;

    #make the underlying easysearch object forget all its preconceptions
}



# Convert a set of oldstyle SB Restrictions to Clauses for RQL

sub _RestrictionsToClauses {
    my $self = shift;

    my %clause;
    foreach my $row ( keys %{ $self->{'AssetRestrictions'} } ) {
        my $restriction = $self->{'AssetRestrictions'}{$row};

        # We need to reimplement the subclause aggregation that SearchBuilder does.
        # Default Subclause is ALIAS.FIELD, and default ALIAS is 'main',
        # Then SB AND's the different Subclauses together.

        # So, we want to group things into Subclauses, convert them to
        # SQL, and then join them with the appropriate DefaultEA.
        # Then join each subclause group with AND.

        my $field = $restriction->{'FIELD'};
        my $realfield = $field;    # CustomFields fake up a fieldname, so
                                   # we need to figure that out

        # One special case
        # Rewrite LinkedTo meta field to the real field
        if ( $field =~ /LinkedTo/ ) {
            $realfield = $field = $restriction->{'TYPE'};
        }

        # Two special case
        # Handle subkey fields with a different real field
        if ( $field =~ /^(\w+)\./ ) {
            $realfield = $1;
        }

        die "I don't know about $field yet"
            unless ( exists $FIELD_METADATA{$realfield}
                or $restriction->{CUSTOMFIELD} );

        my $type = $FIELD_METADATA{$realfield}->[0];
        my $op   = $restriction->{'OPERATOR'};

        my $value = (
            grep    {defined}
                map { $restriction->{$_} } qw(VALUE ASSET BASE TARGET)
        )[0];

        # this performs the moral equivalent of defined or/dor/C<//>,
        # without the short circuiting.You need to use a 'defined or'
        # type thing instead of just checking for truth values, because
        # VALUE could be 0.(i.e. "false")

        # You could also use this, but I find it less aesthetic:
        # (although it does short circuit)
        #( defined $restriction->{'VALUE'}? $restriction->{VALUE} :
        # defined $restriction->{'ASSET'} ?
        # $restriction->{ASSET} :
        # defined $restriction->{'BASE'} ?
        # $restriction->{BASE} :
        # defined $restriction->{'TARGET'} ?
        # $restriction->{TARGET} )

        my $ea = $restriction->{ENTRYAGGREGATOR}
            || $DefaultEA{$type}
            || "AND";
        if ( ref $ea ) {
            die "Invalid operator $op for $field ($type)"
                unless exists $ea->{$op};
            $ea = $ea->{$op};
        }

        # Each CustomField should be put into a different Clause so they
        # are ANDed together.
        if ( $restriction->{CUSTOMFIELD} ) {
            $realfield = $field;
        }

        exists $clause{$realfield} or $clause{$realfield} = [];

        # Escape Quotes
        $field =~ s!(['\\])!\\$1!g;
        $value =~ s!(['\\])!\\$1!g;
        my $data = [ $ea, $type, $field, $op, $value ];

        # here is where we store extra data, say if it's a keyword or
        # something.  (I.e. "TYPE SPECIFIC STUFF")

        if (lc $ea eq 'none') {
            $clause{$realfield} = [ $data ];
        } else {
            push @{ $clause{$realfield} }, $data;
        }
    }
    return \%clause;
}



=head2 _ProcessRestrictions PARAMHASH

# The new _ProcessRestrictions is somewhat dependent on the SQL stuff,
# but isn't quite generic enough to move into Assets_SQL.

=cut

sub _ProcessRestrictions {
    my $self = shift;
    
    #Blow away asset aliases since we'll need to regenerate them for
    #a new search
    delete $self->{'AssetAliases'};
    delete $self->{'items_array'};
    delete $self->{'item_map'};
    delete $self->{'raw_rows'};
    delete $self->{'rows'};
    delete $self->{'count_all'};
 
    my $sql = $self->Query;    # Violating the _SQL namespace
    if ( !$sql || $self->{'RecalcAssetLimits'} ) {

        local $self->{using_restrictions};
        #  "Restrictions to Clauses Branch\n";
        my $clauseRef = eval { $self->_RestrictionsToClauses; };
        if ($@) {
	    $RT::Logger->error( "RestrictionsToClauses: " . $@ );
	    $self->FromSQL("");
        }
        else {
	    $sql = $self->ClausesToSQL($clauseRef);
	    $self->FromSQL($sql) if $sql;
        }
    }

    $self->{'RecalcAssetLimits'} = 0;

}

=head2 _BuildItemMap

Build up a L</ItemMap> of first/last/next/prev items, so that we can
display search nav quickly.

=cut

sub _BuildItemMap {
    my $self = shift;

    my $window = RT->Config->Get('AssetsItemMapSize');

    $self->{'item_map'} = {};

    my $items = $self->ItemsArrayRefWindow( $window );
    return unless $items && @$items;

    my $prev = 0;
    $self->{'item_map'}{'first'} = $items->[0]->Id;
    for ( my $i = 0; $i < @$items; $i++ ) {
        my $item = $items->[$i];
        my $id = $item->Id;
        $self->{'item_map'}{$id}{'defined'} = 1;
        $self->{'item_map'}{$id}{'prev'}    = $prev;
        $self->{'item_map'}{$id}{'next'}    = $items->[$i+1]->Id
            if $items->[$i+1];
        $prev = $id;
    }
    $self->{'item_map'}{'last'} = $prev
        if !$window || @$items < $window;
}

=head2 ItemMap

Returns an a map of all items found by this search. The map is a hash
of the form:

    {
        first => <first asset id found>,
        last => <last asset id found or undef>,

        <asset id> => {
            prev => <the asset id found before>,
            next => <the asset id found after>,
        },
        <asset id> => {
            prev => ...,
            next => ...,
        },
    }

=cut

sub ItemMap {
    my $self = shift;
    $self->_BuildItemMap unless $self->{'item_map'};
    return $self->{'item_map'};
}




=head2 PrepForSerialization

You don't want to serialize a big assets object, as
the {items} hash will be instantly invalid _and_ eat
lots of space

=cut

sub PrepForSerialization {
    my $self = shift;
    delete $self->{'items'};
    delete $self->{'items_array'};
    $self->RedoSearch();
}


sub Export {
    my ($self, $format) = @_;
}

sub ExportExcel {
    my ($self, $format) = @_;
    my $class = 'RTx__AssetTracker__Asset';

    local $HTML::Mason::Commands::r = FakeRequest->new;

    my $row_data = '';
    my $row_count = $self->Count()+1;
    my $export_format = [ { attribute => 'id' }, grep { ($_->{title}||'') ne 'NEWLINE' && $_->{attribute} ne 'id' } @$format ];
    my $column_count = @$export_format;

    my $i = 1;
    my $header = '<Row>';
    for my $f (@$export_format) {
        my $attr = $f->{attribute};
        $row_data .= qq{<Column ss:Width="60.0" />\n};
        my $value = run_component("/Elements/ColumnMap", Class => $class, Name => $attr, Attr => "title") || $attr;
        my $out = _xml_escape_value(
            HTML::Mason::Commands::ProcessColumnMapValue( $value, Arguments => [ $attr ], Escape => 0 ) );
        $header .= qq{<Cell ><Data ss:Type="String">$out</Data></Cell>\n};
    }
    $header .= '</Row>';
    $row_data .= $header;

    while (my $asset = $self->Next) {
        my $row = "\n<Row>";
        my $record = $asset->Export;

        for my $f (@$export_format) {
            my $attr = $f->{attribute};
            my $style = run_component("/Elements/ColumnMap", Class => $class, Name => $attr, Attr => "export_style") || 'Default';
            my $type  = run_component("/Elements/ColumnMap", Class => $class, Name => $attr, Attr => "export_type")  || 'String';
            my $value = run_component("/Elements/ColumnMap", Class => $class, Name => $attr, Attr => "export_value")
                     || run_component("/Elements/ColumnMap", Class => $class, Name => $attr, Attr => "value");
            my $out = _xml_escape_value(
                HTML::Mason::Commands::ProcessColumnMapValue( $value, Arguments => [ $asset ], Escape => 0 ) ) || '';
            $row .= qq{<Cell ss:StyleID="$style"><Data ss:Type="$type">$out</Data></Cell>\n};
        }
        
        $row .= '</Row>';
        $row_data .= $row;
    }


    return <<"EOF"
<?xml version="1.0"?>
<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:o="urn:schemas-microsoft-com:office:office"
 xmlns:x="urn:schemas-microsoft-com:office:excel"
 xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
 xmlns:html="http://www.w3.org/TR/REC-html40">
 <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">
  <Version>12.0</Version>
 </DocumentProperties>
 <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">
  <Date1904/>
 </ExcelWorkbook>
 <Styles>
  <Style ss:ID="Default" ss:Name="Normal">
   <Alignment ss:Vertical="Bottom"/>
   <Borders/>
   <Font ss:FontName="Verdana"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
  <Style ss:ID="s21">
   <NumberFormat ss:Format="General Date"/>
  </Style>
  <Style ss:ID="s23">
   <NumberFormat ss:Format="0"/>
  </Style>
  <Style ss:ID="s24">
   <Alignment ss:Vertical="Bottom" ss:WrapText="1"/>
   <Borders/>
   <Font ss:FontName="Verdana"/>
   <Interior/>
   <NumberFormat/>
   <Protection/>
  </Style>
 </Styles>
 <Worksheet ss:Name="Export">
  <Table ss:ExpandedColumnCount="$column_count" ss:ExpandedRowCount="$row_count" x:FullColumns="1" x:FullRows="1">
$row_data
  </Table>
 </Worksheet>
</Workbook>
EOF

}

sub ImportXML {
    my ($self, $xml, $runscrips, $detailed) = @_;

    return ['permission denied']
        unless $self->CurrentUser->HasRight( Object => $RT::System, Right => 'AssetImport');

    $runscrips = 1 unless defined $runscrips;

    my $dom = create_dom_from_xml($xml);
    my ($first_worksheet) = grep { ref($_) eq 'RTx::AssetTracker::Assets::Worksheet' } @{ $dom->[0]{Kids} };
    my ($first_table) = grep { ref($_) eq 'RTx::AssetTracker::Assets::Table' } @{ $first_worksheet->{Kids} };
    my @rows = grep { ref($_) eq 'RTx::AssetTracker::Assets::Row' } @{ $first_table->{Kids} };
    my $headers = $self->_import_headers(shift @rows);
    @rows = map { $self->_import_row($_, scalar(@$headers)) } @rows;
    my ($rv, $msgs) = $self->Import($headers, \@rows, $runscrips, $detailed);
    return $rv, $msgs;
}

sub Import {
    my ($self, $headers, $rows, $runscrips, $detailed) = @_;

    return 0, ['permission denied']
        unless $self->CurrentUser->HasRight( Object => $RT::System, Right => 'AssetImport');

    $runscrips = 1 unless defined $runscrips;

    my $rv = 0;
    my $msgs = [];
    my $ids = [];

    unless ($headers->[0] eq 'id') {
        push @$msgs, "First column of import must be 'id'";
        return $rv, $msgs;
    }


    $RT::Handle->BeginTransaction();
    my @new = ();
    my @update = ();

    my $error = 0;
    my $n = 1;
    for my $row (@$rows) {
        $n++;
        next unless @$row;

        my ($aid, $msg) = $self->_import($headers, $row, $runscrips, $detailed);
        #warn $row->[0], $msg;

        if (!$aid) {
            push @$msgs, $self->loc("Row [_1]: [_2]", $n, $msg);
            $error++;
        }
        elsif ($row->[0] =~ /^(\d+)(\.0)?$/) {
            push @update, $1;
            push @$ids, $1;
        }
        else {
            push @new, $aid;
            push @$ids, $aid;
        }
    }

    if ($error) {
    $RT::Logger->info( "Asset import error. Rolling back DB transactions." );
        $RT::Handle->Rollback();
        return 0, $msgs;
    }
    else {
        $RT::Handle->Commit();
        #load each asset and do a Create/Update transaction with $runscrips set
        for my $id (@new) {
            my $asset = RTx::AssetTracker::Asset->new($self->CurrentUser);
            $asset->Load($id);
            $asset->_NewTransaction(Type => "Create");
            push @$msgs, $self->loc("Asset #[_1] created", $id);
        }
        for my $id (@update) {
            my $asset = RTx::AssetTracker::Asset->new($self->CurrentUser);
            $asset->Load($id);
            $asset->_NewTransaction(Type => "Update");
            push @$msgs, $self->loc("Asset [_1] possibly updated", $id);
        }
    }

    return $ids, $msgs;

}

sub _import {
    my ($self, $header, $asset_row, $runscrips, $detailed) = @_;

    require Storable;
    my $row = Storable::dclone($asset_row);

    my %asset = ();
    for (@$header) {
        $asset{$_} = shift @$row;
    }

    %asset = $self->_fixup_import(%asset);

    my $id = delete $asset{id}
        or return 0, "id not defined";
    my $asset = RTx::AssetTracker::Asset->new($self->CurrentUser);
    if ($id eq 'new') {
        my ($aid, undef, $err) = $asset->Create( %asset, _Commit => 0, _RecordTransaction => 0 );
        return $aid, $err;
    }
    elsif ($id =~ /(\d+)/) {
        $asset->Load($1);
        return 0, "Nonexistent id: $id" unless $asset->Id;

        my ($aid, undef, $err) = $asset->UpdateAsset( %asset, _Commit => 0, _RecordTransaction => 0, _Detailed => $detailed );
        return $aid, $self->loc("Asset #[_1] not updated: [_2]", $asset->Id, $err);
    }
    else {
        return 0, "Unrecognized id: $id";
    }
}

sub _fixup_import {
    my ($self, %asset) = @_;

    my %fixed;
    map { $fixed{$_} = delete $asset{$_} if defined $asset{$_} } qw(id Name Type Status Description);

    #roles
    foreach my $type ( RTx::AssetTracker::Type->RoleGroupTypes() ) {
        next unless $asset{$type};
        $fixed{$type} = [ split(/,\s*/, delete $asset{$type}) ];
    }

    #links
    for my $type ( keys %RT::Link::TYPEMAP ) {
        next unless exists $asset{$type} && defined $asset{$type};
        my @URIs = split(/,/, delete $asset{$type});
        $fixed{$type} = \@URIs;
    }

    #ip addresses
    my @ips = (exists $asset{'IP Address'} && defined $asset{'IP Address'}) ? split(/\|/, delete $asset{'IP Address'}) : ();
    my $import_ips = [];
    for my $ip (@ips) {
        my ($interface, $address, $mac, $tcp, $udp) = split(/:/, $ip);
        my $tcp_ports = [ split(/,/, $tcp) ];
        my $udp_ports = [ split(/,/, $udp) ];
        push @$import_ips, { 
                 IP        => $address,
                 Interface => $interface,
                 MAC => $mac,
                 TCPPorts => $tcp_ports,
                 UDPPorts => $udp_ports, };
    }
    if (@$import_ips) {
        $fixed{'IP Address'} = $import_ips;
    }

    unless ( $fixed{'Type'} ) {
        my $a = RTx::AssetTracker::Asset->new($self->CurrentUser);
        $a->Load($fixed{'id'});
        $fixed{'Type'} = $a->Type
            if defined($a->Type);
    }

    #custom fields are last
    for my $possible_cf (keys %asset) {
        my $cf = RT::CustomField->new($self->CurrentUser);
        $cf->LoadByNameAndAssetType( Name => $possible_cf, Type => $fixed{'Type'} );
        unless ( $cf->id ) {
            $cf->LoadByNameAndAssetType( Name => $possible_cf, Type => '0' );
        }
        if ($cf->id) {
            my $type = $cf->Type;
            if ($cf->MaxValues && $type =~ /^(Wikitext|Text|Freeform|Autocomplete)$/) {
                $asset{$possible_cf} =~ s/\r\n?/\n/g;
                $fixed{"CustomField-".$cf->id} = delete $asset{$possible_cf};
            }
            elsif ( !$cf->MaxValues && $type =~ /^(Freeform|Autocomplete)$/) {
                $asset{$possible_cf} =~ s/\r\n?/\n/g;
                my @values = split('\n', $asset{$possible_cf});
                $asset{$possible_cf} = \@values;
                $fixed{"CustomField-".$cf->id} = delete $asset{$possible_cf};
            }
            elsif ( $cf->MaxValues && $type eq 'Select') {
#make sure the value is valid???
                $fixed{"CustomField-".$cf->id} = delete $asset{$possible_cf};
            }
            elsif (!$cf->MaxValues && $type eq 'Select') {
#make sure the values are valid???
                $fixed{"CustomField-".$cf->id} = delete $asset{$possible_cf};
            }
            else {
                #????
            }
        }
    }

    #should we error on anything that is left?

    return %fixed;
}

sub _import_headers {
    my ($self, $row) = @_;

    my @headers;

    ROW:
        for my $cell (@{$row->{Kids}}) {
            next unless ref($cell) eq 'RTx::AssetTracker::Assets::Cell';
            for my $data (@{$cell->{Kids}}) {
                next unless ref($data) eq 'RTx::AssetTracker::Assets::Data';
                for my $characters (@{$data->{Kids}}) {
                    next unless ref($characters) eq 'RTx::AssetTracker::Assets::Characters';
                    my $text = $characters->{Text};
                    last ROW unless defined $text;
                    push @headers, $text;
                }
            }
        }

    return \@headers;
}

sub _import_row {
    my ($self, $row, $column_count) = @_;

    my @row;
    my $count = 0;

    ROW:
    for my $cell (@{$row->{Kids}}) {
        next unless ref($cell) eq 'RTx::AssetTracker::Assets::Cell';
        for my $data (@{$cell->{Kids}}) {
            if ( ref($data->{Kids}) && @{$data->{Kids}} ) {
                for my $characters (@{$data->{Kids}}) {
                    my $text = $characters->{Text};
                    push @row, $text;
                }
            }
            else { push @row, undef; }
        }
        last if ++$count == $column_count;
    }

    return \@row;
}


sub create_dom_from_xml {
    my ($xml) = @_;

    my $xp = XML::Parser->new(Style => "Objects", Namespaces => 1);
    my $dom = eval { $xp->parse($xml); };

    unless ($dom && !$@) {
        die "XML document not properly formed: $@\n"
    }

    return $dom;
}


sub _xml_escape_value {
    my ($str) = @_;

    return unless $str;

    $str =~ s/&/&amp;/g;
    $str =~ s/</&lt;/g;
    $str =~ s/>/&gt;/g;
    $str =~ s/"/&quot;/g;
    $str =~ s/\n/&#13;/g;

    return $str;
}

{
    my $mason;
    my $outbuf = '';
    my $data_dir = '';

    use HTML::Mason::FakeApache;
    sub mason {
        unless ($mason) {
            # user may not have permissions on the data directory, so create a
            # new one
            $data_dir = tempdir(CLEANUP => 1);

            $mason = HTML::Mason::Interp->new(
                #RT::Interface::Web::Handler->DefaultHandlerArgs,
                comp_root => [
                    [ local    => $RT::MasonLocalComponentRoot ],
                    (map {[ "plugin-".$_->Name =>  $_->ComponentRoot ]} @{RT->Plugins}),
                    [ standard => $RT::MasonComponentRoot ]
                ],
                request_class        => 'RT::Interface::Web::Request',
                cgi_request => new HTML::Mason::FakeApache,
                auto_send_headers => 0,

                out_method => \$outbuf,
                autohandler_name => '', # disable forced login and more
                data_dir => $data_dir,
            );
        }
        return $mason;
    }

    sub run_component {
        my $x = mason->exec(@_);
        my $ret = $outbuf;
        $outbuf = '';
        #return $ret;
        return $x;
    }
}

package FakeRequest;
sub new { bless {}, shift }
sub header_out { shift }
sub headers_out { shift }
sub content_type {
    my $self = shift;
    $self->{content_type} = shift if @_;
    return $self->{content_type};
}

package RTx::AssetTracker::Assets;


=head1 FLAGS

RTx::AssetTracker::Assets supports several flags which alter search behavior:


allow_deleted_search  (Otherwise never show deleted assets in search results)

These flags are set by calling

$assets->{'flagname'} = 1;

BUG: There should be an API for this

=cut



=head2 NewItem

Returns an empty new RTx::AssetTracker::Asset item

=cut

sub NewItem {
    my $self = shift;
    return(RTx::AssetTracker::Asset->new($self->CurrentUser));
}

RT::Base->_ImportOverlays();

1;
