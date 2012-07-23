=head1 NAME

  RTx::AssetTracker::Assets - a collection of AssetTracker Assets objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Assets;

=head1 DESCRIPTION


=head1 METHODS

=begin testing 

ok (require RTx::AssetTracker::Assets);

=end testing

=cut


# most logic "borrowed" from RT::Tickets rev 2814
package RTx::AssetTracker::Assets;

use strict;
no warnings qw(redefine);
use vars qw( @SORTFIELDS %FIELDS );
use RT::CustomFields;
use File::Temp 'tempdir';
use HTML::Mason;
use XML::Parser;

# FIELDS is a mapping of searchable Field name, to Type, and other
# metadata.

%FIELDS =
  ( Name            => ['STRING',],
    Status          => ['ENUM'],
    Type            => ['ENUM' => 'Type',],
    Creator         => ['ENUM' => 'User',],
    LastUpdatedBy   => ['ENUM' => 'User',],
    id              => ['INT',],
    URI             => ['STRING',],
#    ComponentOf     => [ 'LINK' => To => 'ComponentOf', ],
#    RunsOn          => [ 'LINK' => To => 'RunsOn', ],
#    DependsOn       => [ 'LINK' => To => 'DependsOn', ],
#    RefersTo        => [ 'LINK' => To => 'RefersTo', ],
#    IsRunning       => [ 'LINK' => From => 'RunsOn', ],
#    HasComponent    => [ 'LINK' => From => 'ComponentOf', ],
#    HasComponents   => [ 'LINK' => From => 'ComponentOf', ],
#    DependentOn     => [ 'LINK' => From => 'DependsOn', ],
#    DependedOnBy    => [ 'LINK' => From => 'DependsOn', ],
#    ReferredToBy    => [ 'LINK' => From => 'RefersTo', ],
    LastUpdated     => ['DATE' => 'LastUpdated',],
    Created         => ['DATE' => 'Created',],
    Description     => ['STRING',],
    Content         => ['TRANSFIELD',],
    ContentType     => ['TRANSFIELD',],
    Filename        => ['TRANSFIELD',],
    IP              => ['IPFIELD',],
    MAC             => ['IPFIELD',],
    Interface       => ['IPFIELD',],
    Port            => ['PORTFIELD',],
    Transport       => ['PORTFIELD',],
    TransactionDate => ['TRANSDATE',],
    Updated         => ['TRANSDATE',],
    Owner           => ['WATCHERFIELD' => 'Owner',],
    Admin           => ['WATCHERFIELD' => 'Admin',],
    Watcher         => ['WATCHERFIELD'],
    LinkedTo        => ['LINKFIELD',],
    CustomFieldValue =>['CUSTOMFIELD',],
    CF              => ['CUSTOMFIELD',],
    OwnerGroup      => [ 'MEMBERSHIPFIELD' => 'Owner', ],
    AdminGroup      => [ 'MEMBERSHIPFIELD' => 'Admin', ],
    WatcherGroup     => [ 'MEMBERSHIPFIELD', ],
    %FIELDS,
  );


#sub RegisterLinkField {
#
#    my $base   = shift;
#    my $target = shift;
#
#    $FIELDS{$base}   = [ 'LINK' => To   => $base ];
#    $FIELDS{$target} = [ 'LINK' => From => $base ];
#
#    {
#        no strict 'refs';
#
#        my $limit_base   = "Limit$base";
#        my $limit_target = "Limit$target";
#
#        *$limit_base = sub {
#            my $self = shift;
#            my $asset_uri = shift;
#            $self->LimitLinkedTo ( TARGET => $asset_uri,
#                                   TYPE => $base,          );
#
#        };
#        *$limit_target = sub {
#            my $self = shift;
#            my $asset_uri = shift;
#            $self->LimitLinkedFrom ( BASE => $asset_uri,
#                                     TYPE => $base,        );
#
#        };
#
#    }
#
#}


# Mapping of Field Type to Function
my %dispatch = (
    ENUM            => \&_EnumLimit,
    INT             => \&_IntLimit,
    LINK            => \&_LinkLimit,
    DATE            => \&_DateLimit,
    STRING          => \&_StringLimit,
    TRANSFIELD      => \&_TransLimit,
    TRANSDATE       => \&_TransDateLimit,
    WATCHERFIELD    => \&_WatcherLimit,
    MEMBERSHIPFIELD => \&_WatcherMembershipLimit,
    LINKFIELD       => \&_LinkFieldLimit,
    CUSTOMFIELD     => \&_CustomFieldLimit,
);
my %can_bundle = ( WATCHERFIELD => "yes", );

$dispatch{IPFIELD} = \&_IPLimit;
$dispatch{PORTFIELD} = \&_PortLimit;


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
);

$DefaultEA{IPFIELD} = 'OR';

# Helper functions for passing the above lexically scoped tables above
# into Tickets_Overlay_SQL.
sub FIELDS   { return \%FIELDS }
sub dispatch { return \%dispatch }
sub can_bundle { return \%can_bundle }

# Bring in the clowns.
require RTx::AssetTracker::Assets_Overlay_SQL;

# {{{ sub SortFields

@SORTFIELDS = qw(id Status Type Description Owner Created LastUpdated ); # Owner?

=head2 SortFields

Returns the list of fields that lists of assets can easily be sorted by

=cut

sub SortFields {
        my $self = shift;
        return(@SORTFIELDS);
}


# }}}


# BEGIN SQL STUFF *********************************

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
      unless $op eq "=" or $op eq "!=";

    my $meta = $FIELDS{$field};
    if ( defined $meta->[1] ) {
        my $class = "RTx::AssetTracker::" . $meta->[1];
        my $o     = $class->new( $sb->CurrentUser );
        $o->Load($value);
        $value = $o->Id;
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
        FIELD => $field,
        VALUE => $value,
        OPERATOR => $op,
        @rest,
    );
}

=head2 _LinkLimit

Handle fields which deal with links between tickets.  (MemberOf, DependsOn)

Meta Data:
  1: Direction (From,To)
  2: Link Type (MemberOf, DependsOn,RefersTo)

=cut

sub _LinkLimit {
    my ( $sb, $field, $op, $value, @rest ) = @_;

    my $meta = $FIELDS{$field};
    die "Invalid Operator $op for $field" unless $op =~ /^(=|!=|IS)/io;

    die "Incorrect Metadata for $field"
      unless ( defined $meta->[1] and defined $meta->[2] );

    my $direction = $meta->[1];

    my $matchfield;
    my $linkfield;
    my $is_local = 1;
    my $is_null  = 0;
    if ( $direction eq 'To' ) {
        $matchfield = "Target";
        $linkfield  = "Base";

    }
    elsif ( $direction eq 'From' ) {
        $linkfield  = "Target";
        $matchfield = "Base";

    }
    else {
        die "Invalid link direction '$meta->[1]' for $field\n";
    }

    if ( $op eq '=' || $op =~ /^is/oi ) {
        if ( $value eq '' || $value =~ /^null$/io ) {
            $is_null = 1;
        }
        elsif ( $value =~ /^\d+$/o ) {
            $is_local = 1;
        }
        else {
            $is_local = 0;
        }
    }

#For doing a left join to find "unlinked tickets" we want to generate a query that looks like this
#    SELECT main.* FROM Tickets main
#        LEFT JOIN Links Links_1 ON (     (Links_1.Type = 'MemberOf')
#                                      AND(main.id = Links_1.LocalTarget))
#        WHERE (Links_1.Base IS NULL);

    if ($is_null) {
        my $linkalias = $sb->Join(
            TYPE   => 'left',
            ALIAS1 => 'main',
            FIELD1 => 'URI',
            TABLE2 => 'Links',
            FIELD2 => $linkfield
        );

        $sb->SUPER::Limit(
            LEFTJOIN => $linkalias,
            FIELD    => 'Type',
            OPERATOR => '=',
            VALUE    => $meta->[2],
            @rest,
        );

        $sb->_SQLLimit(
            ALIAS           => $linkalias,
            ENTRYAGGREGATOR => 'AND',
            FIELD           => ( $is_local ? "Local$matchfield" : $matchfield ),
            OPERATOR        => 'IS',
            VALUE           => 'NULL',
            QUOTEVALUE      => '0',
        );

    }
    else {

        $sb->{_sql_linkalias} = $sb->NewAlias('Links')
          unless defined $sb->{_sql_linkalias};

        $sb->_OpenParen();

        $sb->_SQLLimit(
            ALIAS    => $sb->{_sql_linkalias},
            FIELD    => 'Type',
            OPERATOR => '=',
            VALUE    => $meta->[2],
            @rest,
        );

        $sb->_SQLLimit(
            ALIAS           => $sb->{_sql_linkalias},
            ENTRYAGGREGATOR => 'AND',
            FIELD           => ( $is_local ? "Local$matchfield" : $matchfield ),
            OPERATOR        => '=',
            VALUE           => $value,
        );

        #If we're searching on target, join the base to ticket.id
        $sb->_SQLJoin(
            ALIAS1 => 'main',
            FIELD1 => 'URI',
            ALIAS2 => $sb->{_sql_linkalias},
            FIELD2 => ( $is_local ? "Local$linkfield" : $linkfield ),
        );

        $sb->_CloseParen();
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

    my $meta = $FIELDS{$field};
    die "Incorrect Meta Data for $field"
        unless ( defined $meta->[1] );

    use POSIX 'strftime';

    my $date = RT::Date->new( $sb->CurrentUser );
    $date->Set( Format => 'unknown', Value => $value );
    my $time = $date->Unix;

    if ( $op eq "=" ) {

        # if we're specifying =, that means we want everything on a
        # particular single day.  in the database, we need to check for >
        # and < the edges of that day.

        my $daystart = strftime( "%Y-%m-%d %H:%M",
            gmtime( $time - ( $time % 86400 ) ) );
        my $dayend = strftime( "%Y-%m-%d %H:%M",
            gmtime( $time + ( 86399 - $time % 86400 ) ) );

        $sb->_OpenParen;

        $sb->_SQLLimit(
            FIELD    => $meta->[1],
            OPERATOR => ">=",
            VALUE    => $daystart,
            @rest,
        );

        $sb->_SQLLimit(
            FIELD    => $meta->[1],
            OPERATOR => "<=",
            VALUE    => $dayend,
            @rest,
            ENTRYAGGREGATOR => 'AND',
        );

        $sb->_CloseParen;

    }
    else {
        $value = strftime( "%Y-%m-%d %H:%M", gmtime($time) );
        $sb->_SQLLimit(
            FIELD    => $meta->[1],
            OPERATOR => $op,
            VALUE    => $value,
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

  my ($sb,$field,$op,$value,@rest) = @_;

  # See the comments for TransLimit, they apply here too

  $sb->{_sql_ipalias} = $sb->NewAlias ('AT_IPs')
    unless defined $sb->{_sql_ipalias};

  #$sb->_OpenParen;

  $sb->_SQLLimit( ALIAS => $sb->{_sql_ipalias}, FIELD => $field, OPERATOR => $op, VALUE => $value, @rest);

  # Join IPs to Assets
  $sb->_SQLJoin( ALIAS1 => 'main', FIELD1 => $sb->{'primary_key'}, # UGH!
             ALIAS2 => $sb->{_sql_ipalias}, FIELD2 => 'Asset');

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

    $self->{_sql_ipalias} = $self->NewAlias ('AT_IPs')
      unless defined $self->{_sql_ipalias};

    $self->{_sql_portalias} = $self->NewAlias ('AT_Ports')
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

    $sb->{_sql_transalias} = $sb->NewAlias('Transactions')
        unless defined $sb->{_sql_transalias};

    my $date = RT::Date->new( $sb->CurrentUser );
    $date->Set( Format => 'unknown', Value => $value );
    my $time = $date->Unix;

    $sb->_OpenParen;
    if ( $op eq "=" ) {

        # if we're specifying =, that means we want everything on a
        # particular single day.  in the database, we need to check for >
        # and < the edges of that day.

        my $daystart = strftime( "%Y-%m-%d %H:%M",
            gmtime( $time - ( $time % 86400 ) ) );
        my $dayend = strftime( "%Y-%m-%d %H:%M",
            gmtime( $time + ( 86399 - $time % 86400 ) ) );

        $sb->_SQLLimit(
            ALIAS         => $sb->{_sql_transalias},
            FIELD         => 'Created',
            OPERATOR      => ">=",
            VALUE         => $daystart,
            CASESENSITIVE => 0,
            @rest
        );
        $sb->_SQLLimit(
            ALIAS         => $sb->{_sql_transalias},
            FIELD         => 'Created',
            OPERATOR      => "<=",
            VALUE         => $dayend,
            CASESENSITIVE => 0,
            @rest,
            ENTRYAGGREGATOR => 'AND',
        );

    }

    # not searching for a single day
    else {

        #Search for the right field
        $sb->_SQLLimit(
            ALIAS         => $sb->{_sql_transalias},
            FIELD         => 'Created',
            OPERATOR      => $op,
            VALUE         => $value,
            CASESENSITIVE => 0,
            @rest
        );
    }

    $sb->_SQLJoin(
        ALIAS1 => 'main',
        FIELD1 => $sb->{'primary_key'},     # UGH!
        ALIAS2 => $sb->{_sql_transalias},
        FIELD2 => 'ObjectId'
    );

    $sb->SUPER::Limit(
        ALIAS => $sb->{_sql_transalias},
        FIELD => 'ObjectType',
        VALUE => 'RTx::AssetTracker::Asset'
    );

    $sb->_CloseParen;
}

=head2 _TransLimit

Limit based on the Content of a transaction or the ContentType.

Meta Data:
  none

=cut

sub _TransLimit {

    # Content, ContentType, Filename

    # If only this was this simple.  We've got to do something
    # complicated here:

    #Basically, we want to make sure that the limits apply to
    #the same attachment, rather than just another attachment
    #for the same ticket, no matter how many clauses we lump
    #on. We put them in TicketAliases so that they get nuked
    #when we redo the join.

    # In the SQL, we might have
    #       (( Content = foo ) or ( Content = bar AND Content = baz ))
    # The AND group should share the same Alias.

    # Actually, maybe it doesn't matter.  We use the same alias and it
    # works itself out? (er.. different.)

    # Steal more from _ProcessRestrictions

    # FIXME: Maybe look at the previous FooLimit call, and if it was a
    # TransLimit and EntryAggregator == AND, reuse the Aliases?

    # Or better - store the aliases on a per subclause basis - since
    # those are going to be the things we want to relate to each other,
    # anyway.

    # maybe we should not allow certain kinds of aggregation of these
    # clauses and do a psuedo regex instead? - the problem is getting
    # them all into the same subclause when you have (A op B op C) - the
    # way they get parsed in the tree they're in different subclauses.

    my ( $self, $field, $op, $value, @rest ) = @_;

    $self->{_sql_transalias} = $self->NewAlias('Transactions')
        unless defined $self->{_sql_transalias};
    $self->{_sql_trattachalias} = $self->NewAlias('Attachments')
        unless defined $self->{_sql_trattachalias};

    $self->_OpenParen;

    #Search for the right field
    $self->_SQLLimit(
        ALIAS         => $self->{_sql_trattachalias},
        FIELD         => $field,
        OPERATOR      => $op,
        VALUE         => $value,
        CASESENSITIVE => 0,
        @rest
    );

    $self->_SQLJoin(
        ALIAS1 => $self->{_sql_trattachalias},
        FIELD1 => 'TransactionId',
        ALIAS2 => $self->{_sql_transalias},
        FIELD2 => 'id'
    );

    # Join Transactions to Tickets
    $self->_SQLJoin(
        ALIAS1 => 'main',
        FIELD1 => $self->{'primary_key'},     # Why not use "id" here?
        ALIAS2 => $self->{_sql_transalias},
        FIELD2 => 'ObjectId'
    );

    $self->SUPER::Limit(
        ALIAS           => $self->{_sql_transalias},
        FIELD           => 'ObjectType',
        VALUE           => 'RTx::AssetTracker::Asset',
        ENTRYAGGREGATOR => 'AND'
    );

    $self->_CloseParen;

}

=head2 _WatcherLimit

Handle watcher limits.  (Requestor, CC, etc..)

Meta Data:
  1: Field to query on


=begin testing


=end testing

=cut

sub _WatcherLimit {
    my $self  = shift;
    my $field = shift;
    my $op    = shift;
    my $value = shift;
    my %rest  = (@_);

    # Find out what sort of watcher we're looking for
    my $fieldname;
    if ( ref $field ) {
        $fieldname = $field->[0]->[0];
    }
    else {
        $fieldname = $field;
        $field = [ [ $field, $op, $value, %rest ] ];    # gross hack
    }
    my $meta = $FIELDS{$fieldname};
    my $type = ( defined $meta->[1] ? $meta->[1] : undef );

    # Owner was ENUM field, so "Owner = 'xxx'" allowed user to
    # search by id and Name at the same time, this is workaround
    # to preserve backward compatibility
    if ( $fieldname eq 'Owner' ) {
        my $flag = 0;
        for my $chunk ( splice @$field ) {
            my ( $f, $op, $value, %rest ) = @$chunk;
            if ( !$rest{SUBKEY} && $op =~ /^!?=$/ ) {
                $self->_OpenParen unless $flag++;
                my $o = RT::User->new( $self->CurrentUser );
                $o->Load($value);
                $value = $o->Id;
                $self->_SQLLimit(
                    FIELD    => 'Owner',
                    OPERATOR => $op,
                    VALUE    => $value,
                    %rest,
                );
            }
            else {
                push @$field, $chunk;
            }
        }
        $self->_CloseParen if $flag;
        return unless @$field;
    }

    my $users = $self->_WatcherJoin($type);

    # If we're looking for multiple watchers of a given type,
    # TicketSQL will be handing it to us as an array of clauses in
    # $field
    $self->_OpenParen;
    for my $chunk (@$field) {
        ( $field, $op, $value, %rest ) = @$chunk;
        $rest{SUBKEY} ||= 'EmailAddress';

        my $re_negative_op = qr[!=|NOT LIKE];
        $self->_OpenParen if $op =~ /$re_negative_op/;

        $self->_SQLLimit(
            ALIAS         => $users,
            FIELD         => $rest{SUBKEY},
            VALUE         => $value,
            OPERATOR      => $op,
            CASESENSITIVE => 0,
            %rest
        );

        if ( $op =~ /$re_negative_op/ ) {
            $self->_SQLLimit(
                ALIAS           => $users,
                FIELD           => $rest{SUBKEY},
                OPERATOR        => 'IS',
                VALUE           => 'NULL',
                ENTRYAGGREGATOR => 'OR',
            );
            $self->_CloseParen;
        }
    }
    $self->_CloseParen;

}

=head2 _WatcherJoin

Helper function which provides joins to a watchers table both for limits
and for ordering.

=cut

sub _WatcherJoin {
    my $self = shift;
    my $type = shift;

    # we cache joins chain per watcher type
    # if we limit by requestor then we shouldn't join requestors again
    # for sort or limit on other requestors
    if ( $self->{'_watcher_join_users_alias'}{ $type || 'any' } ) {
        return $self->{'_watcher_join_users_alias'}{ $type || 'any' };
    }

# we always have watcher groups for ticket
# this join should be NORMAL
# XXX: if we change this from Join to NewAlias+Limit
# then Pg will complain because SB build wrong query.
# Query looks like "FROM (Tickets LEFT JOIN CGM ON(Groups.id = CGM.GroupId)), Groups"
# Pg doesn't like that fact that it doesn't know about Groups table yet when
# join CGM table into Tickets. Problem is in Join method which doesn't use
# ALIAS1 argument when build braces.
    my $groups = $self->Join(
        ALIAS1          => 'main',
        FIELD1          => 'id',
        TABLE2          => 'Groups',
        FIELD2          => 'Instance',
        ENTRYAGGREGATOR => 'AND'
    );
    $self->SUPER::Limit(
        ALIAS           => $groups,
        FIELD           => 'Domain',
        VALUE           => 'RTx::AssetTracker::Asset-Role',
        ENTRYAGGREGATOR => 'AND'
    );
    $self->SUPER::Limit(
        ALIAS           => $groups,
        FIELD           => 'Type',
        VALUE           => $type,
        ENTRYAGGREGATOR => 'AND'
        )
        if ($type);

    my $groupmembers = $self->Join(
        TYPE   => 'LEFT',
        ALIAS1 => $groups,
        FIELD1 => 'id',
        TABLE2 => 'CachedGroupMembers',
        FIELD2 => 'GroupId'
    );

    # XXX: work around, we must hide groups that
    # are members of the role group we search in,
    # otherwise them result in wrong NULLs in Users
    # table and break ordering. Now, we know that
    # RT doesn't allow to add groups as members of the
    # ticket roles, so we just hide entries in CGM table
    # with MemberId == GroupId from results
    $self->SUPER::Limit(
        LEFTJOIN   => $groupmembers,
        FIELD      => 'GroupId',
        OPERATOR   => '!=',
        VALUE      => "$groupmembers.MemberId",
        QUOTEVALUE => 0,
    );
    my $users = $self->Join(
        TYPE   => 'LEFT',
        ALIAS1 => $groupmembers,
        FIELD1 => 'MemberId',
        TABLE2 => 'Users',
        FIELD2 => 'id'
    );
    return $self->{'_watcher_join_users_alias'}{ $type || 'any' } = $users;
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

    # {{{ Tie to groups for tickets we care about
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
    my $meta = $FIELDS{$field};
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

    $self->Join(
        ALIAS1 => $memberships,
        FIELD1 => 'MemberId',
        ALIAS2 => $users,
        FIELD2 => 'id'
    );

    $self->_CloseParen;

}

sub _LinkFieldLimit {
    my $restriction;
    my $self;
    my $LinkAlias;
    my %args;
    if ( $restriction->{'TYPE'} ) {
        $self->SUPER::Limit(
            ALIAS           => $LinkAlias,
            ENTRYAGGREGATOR => 'AND',
            FIELD           => 'Type',
            OPERATOR        => '=',
            VALUE           => $restriction->{'TYPE'}
        );
    }

    #If we're trying to limit it to things that are target of
    if ( $restriction->{'TARGET'} ) {

        # If the TARGET is an integer that means that we want to look at
        # the LocalTarget field. otherwise, we want to look at the
        # "Target" field
        my ($matchfield);
        if ( $restriction->{'TARGET'} =~ /^(\d+)$/ ) {
            $matchfield = "LocalTarget";
        }
        else {
            $matchfield = "Target";
        }
        $self->SUPER::Limit(
            ALIAS           => $LinkAlias,
            ENTRYAGGREGATOR => 'AND',
            FIELD           => $matchfield,
            OPERATOR        => '=',
            VALUE           => $restriction->{'TARGET'}
        );

        #If we're searching on target, join the base to ticket.id
        $self->_SQLJoin(
            ALIAS1 => 'main',
            FIELD1 => $self->{'primary_key'},
            ALIAS2 => $LinkAlias,
            FIELD2 => 'LocalBase'
        );
    }

    #If we're trying to limit it to things that are base of
    elsif ( $restriction->{'BASE'} ) {

        # If we're trying to match a numeric link, we want to look at
        # LocalBase, otherwise we want to look at "Base"
        my ($matchfield);
        if ( $restriction->{'BASE'} =~ /^(\d+)$/ ) {
            $matchfield = "LocalBase";
        }
        else {
            $matchfield = "Base";
        }

        $self->SUPER::Limit(
            ALIAS           => $LinkAlias,
            ENTRYAGGREGATOR => 'AND',
            FIELD           => $matchfield,
            OPERATOR        => '=',
            VALUE           => $restriction->{'BASE'}
        );

        #If we're searching on base, join the target to ticket.id
        $self->_SQLJoin(
            ALIAS1 => 'main',
            FIELD1 => $self->{'primary_key'},
            ALIAS2 => $LinkAlias,
            FIELD2 => 'LocalTarget'
        );
    }
}


=head2 KeywordLimit

Limit based on Keywords

Meta Data:
  none

=cut

sub _CustomFieldLimit {
    my ( $self, $_field, $op, $value, @rest ) = @_;

    my %rest  = @rest;
    my $field = $rest{SUBKEY} || die "No field specified";

    # For our sanity, we can only limit on one type at a time
    my $type = 0;

    if ( $field =~ /^(.+?)\.{(.+)}$/ ) {
        $type = $1;
        $field = $2;
    }
    $field = $1 if $field =~ /^{(.+)}$/;    # trim { }


    # If we're trying to find custom fields that don't match something, we
    # want tickets where the custom field has no value at all.  Note that
    # we explicitly don't include the "IS NULL" case, since we would
    # otherwise end up with a redundant clause.

    my $null_columns_ok;
    if ( ( $op =~ /^NOT LIKE$/i ) or ( $op eq '!=' ) ) {
        $null_columns_ok = 1;
    }

    my $cfid = 0;
    if ($type) {

        my $q = RTx::AssetTracker::Type->new( $self->CurrentUser );
        $q->Load($type) if ($type);

        my $cf;
        if ( $q->id ) {
            $cf = $q->CustomField($field);
        }
        else {
            $cf = RT::CustomField->new( $self->CurrentUser );
            $cf->LoadByNameAndType( Type => '0', Name => $field );
        }

        $cfid = $cf->id;

    }

    my $AssetCFs;
    my $cfkey = $cfid ? $cfid : "$type.$field";

    # Perform one Join per CustomField
    if ( $self->{_sql_object_cf_alias}{$cfid} ) {
        $AssetCFs  = $self->{_sql_object_cf_alias}{$cfid};
    }
    else {
        if ($cfid) {
            $AssetCFs  = $self->{_sql_object_cf_alias}{$cfkey} = $self->Join(
                TYPE   => 'left',
                ALIAS1 => 'main',
                FIELD1 => 'id',
                TABLE2 => 'ObjectCustomFieldValues',
                FIELD2 => 'ObjectId',
            );
            $self->SUPER::Limit(
                LEFTJOIN        => $AssetCFs,
                FIELD           => 'CustomField',
                VALUE           => $cfid,
                ENTRYAGGREGATOR => 'AND'
            );
        }
        else {
            my $cfalias = $self->Join(
                TYPE       => 'left',
                EXPRESSION => "'$field'",
                TABLE2     => 'CustomFields',
                FIELD2     => 'Name',
            );

            $AssetCFs = $self->{_sql_object_cf_alias}{$cfkey} = $self->Join(
                TYPE   => 'left',
                ALIAS1 => $cfalias,
                FIELD1 => 'id',
                TABLE2 => 'ObjectCustomFieldValues',
                FIELD2 => 'CustomField',
            );
            $self->SUPER::Limit(
                LEFTJOIN        => $AssetCFs,
                FIELD           => 'ObjectId',
                VALUE           => 'main.id',
                QUOTEVALUE      => 0,
                ENTRYAGGREGATOR => 'AND',
            );
        }
        $self->SUPER::Limit(
            LEFTJOIN => $AssetCFs,
            FIELD    => 'ObjectType',
            VALUE    => ref( $self->NewItem )
            ,    # we want a single item, not a collection
            ENTRYAGGREGATOR => 'AND'
        );
        $self->SUPER::Limit(
            LEFTJOIN        => $AssetCFs,
            FIELD           => 'Disabled',
            OPERATOR        => '=',
            VALUE           => '0',
            ENTRYAGGREGATOR => 'AND'
        );
    }

    $self->_OpenParen if ($null_columns_ok);

    $self->_SQLLimit(
        ALIAS      => $AssetCFs,
        FIELD      => 'Content',
        OPERATOR   => $op,
        VALUE      => $value,
        QUOTEVALUE => 1,
        @rest
    );

    if ($null_columns_ok) {
        $self->_SQLLimit(
            ALIAS           => $AssetCFs,
            FIELD           => 'Content',
            OPERATOR        => 'IS',
            VALUE           => 'NULL',
            QUOTEVALUE      => 0,
            ENTRYAGGREGATOR => 'OR',
        );
    }
    $self->_CloseParen if ($null_columns_ok);

}

# End Helper Functions

# End of SQL Stuff -------------------------------------------------

# {{{ Limit the result set based on content

# {{{ sub Limit
sub Limit {
    my $self = shift;
    my %args = (
        FIELD       => undef,
        OPERATOR    => '=',
        VALUE       => undef,
        DESCRIPTION => undef,
        @_
    );
    $args{'DESCRIPTION'} = $self->loc( "[_1] [_2] [_3]",
        $args{'FIELD'}, $args{'OPERATOR'}, $args{'VALUE'} )
      if ( !defined $args{'DESCRIPTION'} );

    my $index = $self->_NextIndex;

 #make the AssetRestrictions hash the equivalent of whatever we just passed in;

    %{ $self->{'AssetRestrictions'}{$index} } = %args;

    $self->{'RecalcAssetLimits'} = 1;

# If we're looking at the effective id, we don't want to append the other clause
# which limits us to tickets where id = effective id
    #if ( $args{'FIELD'} eq 'EffectiveId' ) {
        #$self->{'looking_at_effective_id'} = 1;
    #}

    #if ( $args{'FIELD'} eq 'Type' ) {
        #$self->{'looking_at_type'} = 1;
    #}

    return ($index);
}
# }}}

=head2 FreezeLimits

Returns a frozen string suitable for handing back to ThawLimits.

=cut

sub _FreezeThawKeys {
    'AssetRestrictions',
    'restriction_index',
    #'looking_at_effective_id',
    #'looking_at_type'
}

# {{{ sub FreezeLimits

sub FreezeLimits {
    my $self = shift;
    require Storable;
    require MIME::Base64;
    MIME::Base64::base64_encode(
        Storable::freeze( \@{$self}{ $self->_FreezeThawKeys } ) );
}

# }}}

=head2 ThawLimits

Take a frozen Limits string generated by FreezeLimits and make this tickets
object have that set of limits.

=cut
 
# {{{ sub ThawLimits

sub ThawLimits {
    my $self = shift;
    my $in   = shift;

    #if we don't have $in, get outta here.
    return undef unless ($in);

    $self->{'RecalcAssetLimits'} = 1;

    require Storable;
    require MIME::Base64;

    #We don't need to die if the thaw fails.
    @{$self}{ $self->_FreezeThawKeys } =
      eval { @{ Storable::thaw( MIME::Base64::base64_decode($in) ) }; };

    $RT::Logger->error($@) if $@;

}

# }}}


# {{{ Limit by enum or foreign key

# {{{ sub LimitType

=head2 LimitType

LimitType takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of = or !=. (It defaults to =).
VALUE is a queue id or Name.


=cut

sub LimitType {
    my $self = shift;
    my %args = (VALUE => undef,
                OPERATOR => '=',
                @_);

    #TODO  VALUE should also take queue names and queue objects
    #TODO FIXME why are we canonicalizing to name, not id, robrt?
    if ($args{VALUE} !~ /^\d+$/) {
      my $type = new RTx::AssetTracker::Type($self->CurrentUser);
      $type->Load($args{'VALUE'});
      $args{VALUE} = $type->Id;
    }

    # What if they pass in an Id?  Check for isNum() and convert to
    # string.

    #TODO check for a valid queue here

    $self->Limit (FIELD => 'Type',
                  VALUE => $args{VALUE},
                  OPERATOR => $args{'OPERATOR'},
                  DESCRIPTION => join(
                   ' ', $self->loc('Type'), $args{'OPERATOR'}, $args{VALUE},
                  ),
                 );

}
# }}}

# {{{ sub LimitStatus

=head2 LimitStatus

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of = or !=.
VALUE is a status.

=cut

sub LimitStatus {
    my $self = shift;
    my %args = (
        OPERATOR => '=',
        @_
    );
    $self->Limit(
        FIELD       => 'Status',
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        DESCRIPTION => join( ' ',
            $self->loc('Status'), $args{'OPERATOR'},
            $self->loc( $args{'VALUE'} ) ),
    );
}

# }}}

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



# {{{ sub LimitDescription

=head2 LimitDescription

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of = or !=.
VALUE is a string to search for in the description of the asset.

=cut

sub LimitDescription {
    my $self = shift;
    my %args = (@_);
    $self->Limit (FIELD => 'Description',
		  VALUE => $args{'VALUE'},
		  OPERATOR => $args{'OPERATOR'},
		  DESCRIPTION => join(
		   ' ', $self->loc('Description'), $args{'OPERATOR'}, $args{'VALUE'},
		  ),
		 );
}

# }}}

# {{{ sub LimitId

=head2 LimitId

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of =, >, < or !=.
VALUE is a asset Id to search for

=cut

sub LimitId {
    my $self = shift;
    my %args = (
        OPERATOR => '=',
        @_
    );

    $self->Limit(
        FIELD       => 'id',
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        DESCRIPTION =>
          join( ' ', $self->loc('Id'), $args{'OPERATOR'}, $args{'VALUE'}, ),
    );
}

# }}}

# {{{ sub LimitURI

=head2 LimitURI

Takes a paramhash with the fields OPERATOR and VALUE.
OPERATOR is one of =, >, < or !=.
VALUE is a asset URI to search for

=cut

sub LimitURI {
    my $self = shift;
    my %args = (OPERATOR => '=',
                @_);

    $self->Limit (FIELD => 'URI',
                  VALUE => $args{'VALUE'},
                   OPERATOR => $args{'OPERATOR'},
                   DESCRIPTION => join(
                   ' ', $self->loc('Id'), $args{'OPERATOR'}, $args{'VALUE'},
                  ),
                 );
}

# }}}

# {{{ Limiting watchers

# {{{ sub LimitWatcher

=head2 LimitWatcher

  Takes a paramhash with the fields OPERATOR, TYPE and VALUE.
  OPERATOR is one of =, LIKE, NOT LIKE or !=.
  VALUE is a value to match the ticket\'s watcher email addresses against
  TYPE is the sort of watchers you want to match against. Leave it undef if you want to search all of them

=begin testing

my $t1 = RTx::AssetTracker::Asset->new($RT::SystemUser);
$t1->Create(Type => 'Servers', Description => "LimitWatchers test", Owners => \['requestor1@example.com']);

=end testing

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

    $self->Limit(
        FIELD       => $watcher_type,
        VALUE       => $args{'VALUE'},
        OPERATOR    => $args{'OPERATOR'},
        TYPE        => $args{'TYPE'},
        DESCRIPTION => join( ' ',
            $self->loc($watcher_type),
            $args{'OPERATOR'}, $args{'VALUE'}, ),
    );
}

# }}}


# }}}

# {{{ Limiting based on links

# {{{ LimitLinkedTo

=head2 LimitLinkedTo

LimitLinkedTo takes a paramhash with two fields: TYPE and TARGET
TYPE limits the sort of link we want to search on

TYPE = { RefersTo, ComponentOf, DependsOn, RunsOn }

TARGET is the id or URI of the TARGET of the link

=cut

sub LimitLinkedTo {
    my $self = shift;
    my %args = (
        TARGET => undef,
        TYPE   => undef,
        @_
    );

    $self->Limit(
        FIELD       => 'LinkedTo',
        BASE        => undef,
        TARGET      => $args{'TARGET'},
        TYPE        => $args{'TYPE'},
        DESCRIPTION => $self->loc(
            "Assets [_1] by [_2]",
            $self->loc( $args{'TYPE'} ),
            $args{'TARGET'}
        ),
    );
}


# }}}

# {{{ LimitLinkedFrom

=head2 LimitLinkedFrom

LimitLinkedFrom takes a paramhash with two fields: TYPE and BASE
TYPE limits the sort of link we want to search on


BASE is the id or URI of the BASE of the link


=cut

sub LimitLinkedFrom {
    my $self = shift;
    my %args = (
        BASE   => undef,
        TYPE   => undef,
        @_
    );

    $self->Limit(
        FIELD       => 'LinkedTo',
        TARGET      => undef,
        BASE        => $args{'BASE'},
        TYPE        => $args{'TYPE'},
        DESCRIPTION => $self->loc(
            "Assets [_1] [_2]",
            $self->loc( $args{'TYPE'} ),
            $args{'BASE'}
        ),
    );
}


# }}}

# {{{ LimitComponentOf
sub old_LimitComponentOf {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedTo ( TARGET=> "$asset_id",
			   TYPE => 'ComponentOf',
			  );

}
# }}}

# {{{ LimitHasComponent
sub old_LimitHasComponent {
    my $self = shift;
    my $asset_id =shift;
    $self->LimitLinkedFrom ( BASE => "$asset_id",
			     TYPE => 'HasComponent',
			     );

}
# }}}

# {{{ LimitDependsOn

sub old_LimitDependsOn {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedTo ( TARGET => "$asset_id",
                           TYPE => 'DependsOn',
			   );

}

# }}}

# {{{ LimitDependedOnBy

sub old_LimitDependedOnBy {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedFrom (  BASE => "$asset_id",
                               TYPE => 'DependentOn',
			     );

}

# }}}

# {{{ LimitRunsOn

sub old_LimitRunsOn {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedTo ( TARGET => "$asset_id",
                           TYPE => 'RunsOn',
			   );

}

# }}}

# {{{ LimitIsRunning

sub old_LimitIsRunning {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedFrom (  BASE => "$asset_id",
                               TYPE => 'IsRunning',
			     );

}

# }}}


# {{{ LimitRefersTo

sub old_LimitRefersTo {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedTo ( TARGET => "$asset_id",
                           TYPE => 'RefersTo',
			   );

}

# }}}

# {{{ LimitReferredToBy

sub old_LimitReferredToBy {
    my $self = shift;
    my $asset_id = shift;
    $self->LimitLinkedFrom (  BASE=> "$asset_id",
                               TYPE => 'ReferredToBy',
			     );

}

# }}}

# }}}

# {{{ limit based on asset date attribtes

# {{{ sub LimitDate

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
        $args{'DESCRIPTION'} =
            $args{'FIELD'} . " "
          . $args{'OPERATOR'} . " "
          . $args{'VALUE'} . " GMT";
    }

    $self->Limit(%args);

}

# }}}




sub LimitCreated {
    my $self = shift;
    $self->LimitDate( FIELD => 'Created', @_);
}
sub LimitLastUpdated {
    my $self = shift;
    $self->LimitDate( FIELD => 'LastUpdated', @_);
}
#
# {{{ sub LimitTransactionDate

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
        $args{'DESCRIPTION'} =
            $args{'FIELD'} . " "
          . $args{'OPERATOR'} . " "
          . $args{'VALUE'} . " GMT";
    }

    $self->Limit(%args);

}

# }}}

# }}}

# {{{ Limit based on custom fields
# {{{ sub LimitCustomField

=head2 LimitCustomField

Takes a paramhash of key/value pairs with the following keys:

=over 4

=item CUSTOMFIELD - CustomField name or id.  If a name is passed, an additional parameter TYPE may also be passed to distinguish the custom field.

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
        $CF->LoadByNameAndType(
            Name  => $args{CUSTOMFIELD},
            Type => $args{TYPE}
        );
        $args{CUSTOMFIELD} = $CF->Id;
    }

    #If we are looking to compare with a null value.
    if ( $args{'OPERATOR'} =~ /^is$/i ) {
        $args{'DESCRIPTION'} ||=
          $self->loc( "Custom field [_1] has no value.", $CF->Name );
    }
    elsif ( $args{'OPERATOR'} =~ /^is not$/i ) {
        $args{'DESCRIPTION'} ||=
          $self->loc( "Custom field [_1] has a value.", $CF->Name );
    }

    # if we're not looking to compare with a null value
    else {
        $args{'DESCRIPTION'} ||= $self->loc( "Custom field [_1] [_2] [_3]",
            $CF->Name, $args{OPERATOR}, $args{VALUE} );
    }

    my $q = "";
    if ( $CF->Type ) {
        my $qo = new RTx::AssetTracker::Type( $self->CurrentUser );
        $qo->Load( $CF->Type );
        $q = $qo->Name;
    }

    my @rest;
    @rest = ( ENTRYAGGREGATOR => 'AND' )
      if ( $CF->Type eq 'SelectMultiple' );

    $self->Limit(
        VALUE => $args{VALUE},
        FIELD => "CF."
          . (
              $q
            ? $q . ".{" . $CF->Name . "}"
            : $CF->Name
          ),
        OPERATOR    => $args{OPERATOR},
        CUSTOMFIELD => 1,
        @rest,
    );

    $self->{'RecalcAssetLimits'} = 1;
}

# }}}
# }}}

# {{{ sub _NextIndex

=head2 _NextIndex

Keep track of the counter for the array of restrictions

=cut

sub _NextIndex {
    my $self = shift;
    return ( $self->{'restriction_index'}++ );
}
 
# }}}

# }}}

# {{{ Core bits to make this a DBIx::SearchBuilder object

# {{{ sub _Init
sub _Init  {
    my $self = shift;
    $self->{'table'}                   = "AT_Assets";
    $self->{'RecalcAssetLimits'}       = 1;
    #$self->{'looking_at_effective_id'} = 0;
    #$self->{'looking_at_type'}        = 0;
    $self->{'restriction_index'}       = 1;
    $self->{'primary_key'}             = "id";
    delete $self->{'items_array'};
    delete $self->{'item_map'};
    delete $self->{'columns_to_display'};
    $self->SUPER::_Init(@_);

    $self->_InitSQL;

}
# }}}

# {{{ sub Count
sub Count {
    my $self = shift;
    $self->_ProcessRestrictions() if ( $self->{'RecalcAssetLimits'}  == 1 );
    return ( $self->SUPER::Count() );
}
# }}}

# {{{ sub CountAll
sub CountAll {
    my $self = shift;
    $self->_ProcessRestrictions() if ( $self->{'RecalcAssetLimits'}  == 1 );
    return ( $self->SUPER::CountAll() );
}
# }}}


# {{{ sub ItemsArrayRef

=head2 ItemsArrayRef

Returns a reference to the set of all items found in this search

=cut

sub ItemsArrayRef {
    my $self = shift;
    my @items;

    unless ( $self->{'items_array'} ) {

        my $placeholder = $self->_ItemsCounter;
        $self->GotoFirstItem();
        while ( my $item = $self->Next ) {
            push( @{ $self->{'items_array'} }, $item );
        }
        $self->GotoItem($placeholder);
        $self->{'items_array'} = $self->ItemsOrderBy( $self->{'items_array'});
    }
    return ( $self->{'items_array'} );
}
# }}}

# {{{ sub Next
sub Next {
    my $self = shift;
 	
    $self->_ProcessRestrictions() if ( $self->{'RecalcAssetLimits'}  == 1 );

    my $Asset  = $self->SUPER::Next();
    if ( ( defined($Asset ) ) and ( ref($Asset ) ) ) {

        #Make sure we _never_ show deleted tickets
        #TODO we should be doing this in the where clause.
        #but you can't do multiple clauses on the same field just yet :/

        #if ($Asset->__Value('Status') eq 'deleted') {
            #return($self->Next());
        #}

        # Since Asset could be granted with more rights instead
        # of being revoked, it's ok if queue rights allow
        # ShowAsset.  It seems need another query, but we have
        # rights cache in Principal::HasRight.
        if ($Asset->TypeObj->CurrentUserHasRight('ShowAsset')
            || $Asset->CurrentUserHasRight('ShowAsset'))
        {
            return($Asset);
        }

        #If the user doesn't have the right to show this ticket
        else {	
            return ( $self->Next() );
        }
    }

    #if there never was any ticket
    else {
        return (undef);
    }	

}
# }}}

# }}}

# {{{ Deal with storing and restoring restrictions

# {{{ sub LoadRestrictions

=head2 LoadRestrictions

LoadRestrictions takes a string which can fully populate the AssetRestrictons hash.
TODO It is not yet implemented

=cut

# }}}

# {{{ sub DescribeRestrictions

=head2 DescribeRestrictions

takes nothing.
Returns a hash keyed by restriction id.
Each element of the hash is currently a one element hash that contains DESCRIPTION which
is a description of the purpose of that AssetRestriction

=cut

sub DescribeRestrictions  {
    my $self = shift;

    my ( $row, %listing );

    foreach $row ( keys %{ $self->{'AssetRestrictions' } } ) {
	$listing{$row} = $self->{'AssetRestrictions' }{$row}{'DESCRIPTION'};
    }
    return (%listing);
}
# }}}

# {{{ sub RestrictionValues

=head2 RestrictionValues FIELD

Takes a restriction field and returns a list of values this field is restricted
to.

=cut

sub RestrictionValues {
    my $self  = shift;
    my $field = shift;
    map $self->{'AssetRestrictions' }{$_}{'VALUE'}, grep {
             $self->{'AssetRestrictions' }{$_}{'FIELD'}    eq $field
          && $self->{'AssetRestrictions' }{$_}{'OPERATOR'} eq "="
      }
      keys %{ $self->{'AssetRestrictions' } };
}

# }}}

# {{{ sub ClearRestrictions

=head2 ClearRestrictions

Removes all restrictions irretrievably

=cut

sub ClearRestrictions {
    my $self = shift;
    delete $self->{'AssetRestrictions'};
    #$self->{'looking_at_effective_id'} = 0;
    #$self->{'looking_at_type'}         = 0;
    $self->{'RecalcAssetLimits'}        = 1;
}

# }}}

# {{{ sub DeleteRestriction

=head2 DeleteRestriction

Takes the row Id of a restriction (From DescribeRestrictions' output, for example.
Removes that restriction from the session's limits.

=cut


sub DeleteRestriction {
    my $self = shift;
    my $row  = shift;
    delete $self->{'AssetRestrictions' }{$row};

    $self->{'RecalcAssetLimits' } = 1;

    #make the underlying easysearch object forget all its preconceptions
}

# }}}

# {{{ sub _RestrictionsToClauses

# Convert a set of oldstyle SB Restrictions to Clauses for RQL

sub _RestrictionsToClauses {
    my $self = shift;

    my $row;
    my %clause;
    foreach $row ( keys %{ $self->{'AssetRestrictions' } } ) {
        my $restriction = $self->{'AssetRestrictions' }{$row};

        #use Data::Dumper;
        #print Dumper($restriction),"\n";

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
          unless ( exists $FIELDS{$realfield} or $restriction->{CUSTOMFIELD} );

        my $type = $FIELDS{$realfield}->[0];
        my $op   = $restriction->{'OPERATOR'};

        my $value = (
            grep  { defined }
              map { $restriction->{$_} } qw(VALUE ASSET  BASE TARGET)
        )[0];

        # this performs the moral equivalent of defined or/dor/C<//>,
        # without the short circuiting.You need to use a 'defined or'
        # type thing instead of just checking for truth values, because
        # VALUE could be 0.(i.e. "false")

        # You could also use this, but I find it less aesthetic:
        # (although it does short circuit)
        #( defined $restriction->{'VALUE'}? $restriction->{VALUE} :
        # defined $restriction->{'ASSET' } ?
        # $restriction->{ASSET}  :
        # defined $restriction->{'BASE'} ?
        # $restriction->{BASE} :
        # defined $restriction->{'TARGET'} ?
        # $restriction->{TARGET} )

        my $ea = $restriction->{ENTRYAGGREGATOR} || $DefaultEA{$type} || "AND";
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
        $field =~ s!(['"])!\\$1!g;
        $value =~ s!(['"])!\\$1!g;
        my $data = [ $ea, $type, $field, $op, $value ];

        # here is where we store extra data, say if it's a keyword or
        # something.  (I.e. "TYPE SPECIFIC STUFF")

        #print Dumper($data);
        push @{ $clause{$realfield} }, $data;
    }
    return \%clause;
}

# }}}

# {{{ sub _ProcessRestrictions

=head2 _ProcessRestrictions PARAMHASH

# The new _ProcessRestrictions is somewhat dependent on the SQL stuff,
# but isn't quite generic enough to move into Assets_Overlay_SQL.

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
    if ( !$sql || $self->{'RecalcAssetLimits' } ) {

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

    $self->{'RecalcAssetLimits'}  = 0;

}

=head2 _BuildItemMap

    # Build up a map of first/last/next/prev items, so that we can display search nav quickly

=cut

sub _BuildItemMap {
    my $self = shift;

    my $items = $self->ItemsArrayRef;
    my $prev  = 0;

    delete $self->{'item_map'};
    if ( $items->[0] ) {
        #$self->{'item_map'}->{'first'} = $items->[0]->EffectiveId;
        $self->{'item_map'}->{'first'} = $items->[0]->Id;
        while ( my $item = shift @$items ) {
            #my $id = $item->EffectiveId;
            my $id = $item->Id;
            $self->{'item_map'}->{$id}->{'defined'} = 1;
            $self->{'item_map'}->{$id}->{prev}      = $prev;
            #$self->{'item_map'}->{$id}->{next}      = $items->[0]->EffectiveId
            $self->{'item_map'}->{$id}->{next}      = $items->[0]->Id
              if ( $items->[0] );
            $prev = $id;
        }
        $self->{'item_map'}->{'last'} = $prev;
    }
} 


=head2 ItemMap

Returns an a map of all items found by this search. The map is of the form

$ItemMap->{'first'} = first assetid found
$ItemMap->{'last'} = last assetid found
$ItemMap->{$id}->{prev} = the asset id found before $id
$ItemMap->{$id}->{next} = the asset id found after $id

=cut

sub ItemMap {
    my $self = shift;
    $self->_BuildItemMap()
      unless ( $self->{'items_array'} and $self->{'item_map' });
    return ( $self->{'item_map'} );
}

=cut
 

}



# }}}

# }}}

=head2 PrepForSerialization

You don't want to serialize a big assets object, as the {items} hash will be instantly invalid _and_ eat lots of space

=cut

sub PrepForSerialization {
    my $self = shift;
    delete $self->{'items'};
    $self->RedoSearch();
}

sub Export {
    my ($self, $format) = @_;
}

sub ExportExcel {
    my ($self, $format) = @_;

    local $HTML::Mason::Commands::r = FakeRequest->new;

    my $row_data = '';
    my $row_count = $self->Count()+1;
    my $export_format = [ { attribute => 'id' }, grep { $_->{title} ne 'NEWLINE' && $_->{attribute} ne 'id' } @$format ];
    my $column_count = @$export_format;

    my $i = 1;
    my $header = '<Row>';
    for my $f (@$export_format) {
        my $attr = $f->{attribute};
        $row_data .= qq{<Column ss:Width="60.0" />\n};
        my $value = run_component("/Elements/RTx__AssetTracker__Asset/ColumnMap", Name => $attr, Attr => "title") || $attr;
        my $out = _xml_escape_value($value);
        $header .= qq{<Cell ><Data ss:Type="String">$out</Data></Cell>\n};
    }
    $header .= '</Row>';
    $row_data .= $header;

    while (my $asset = $self->Next) {
        my $row = "\n<Row>";
        my $record = $asset->Export;

        for my $f (@$export_format) {
            my $attr = $f->{attribute};
            my $style = run_component("/Elements/RTx__AssetTracker__Asset/ColumnMap", Name => $attr, Attr => "style") || 'Default';
            my $type = run_component("/Elements/RTx__AssetTracker__Asset/ColumnMap", Name => $attr, Attr => "type") || 'String';
            my $value = run_component("/Elements/RTx__AssetTracker__Asset/ColumnMap", Name => $attr, Attr => "export_value")
                     || run_component("/Elements/RTx__AssetTracker__Asset/ColumnMap", Name => $attr, Attr => "value");
            my $out;
            if (ref($value) eq 'CODE') {
                my ($computed_value) = $value->($asset);
                if (ref($computed_value) eq 'SCALAR') {
                    $out = _xml_escape_value($$computed_value);
                }
                else {
                    $out = _xml_escape_value($computed_value);
                }
            }
            elsif (ref($value) eq 'SCALAR') {
                $out = _xml_escape_value($$value);
            }
            else {
                $out = _xml_escape_value($value);
            }
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

    return ['permission denied'] unless RT->Config->Get("AssetImportRequiresRights")
                                 && $self->CurrentUser->HasRight( Object => $RTx::AssetTracker::System, Right => 'AssetImport');

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

    return 0, ['permission denied'] unless RT->Config->Get("AssetImportRequiresRights")
                                    && $self->CurrentUser->HasRight( Object => $RTx::AssetTracker::System, Right => 'AssetImport');

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
    for my $row (@$rows) {
        my ($aid, $msg) = $self->_import($headers, $row, $runscrips, $detailed);
        #warn $row->[0], $msg;

        if (!$aid) {
            push @$msgs, $msg;
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
    $RT::Logger->error( "Asset import error. Rolling back DB transactions." );
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

    my $id = delete $asset{id};
    my $asset = RTx::AssetTracker::Asset->new($self->CurrentUser);
    if ($id eq 'new') {
        my ($aid, undef, $err) = $asset->Create( %asset, _Commit => 0, _RecordTransaction => 0 );
        return $aid, $err;
    }
    elsif ($id =~ /(\d+)/) {
        $asset->Load($1);
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
    $fixed{$_} = delete $asset{$_} for qw(id Name Type Status Description);

    #roles
    foreach my $type ( RTx::AssetTracker::Type->ActiveRoleArray() ) {
        next unless $asset{$type};
        $fixed{$type} = [ split(/,\s*/, delete $asset{$type}) ];
    }

    #links
    my $LINKTYPEMAP = RTx::AssetTracker::Asset::LINKTYPEMAP();
    for my $type (keys %$LINKTYPEMAP) {
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

    #custom fields are last
    for my $possible_cf (keys %asset) {
        my $cf = RT::CustomField->new($self->CurrentUser);
        $cf->LoadByName(Name => $possible_cf);
        if ($cf->id) {
            my $type = $cf->Type;
            if ($cf->MaxValues && $type =~ /^(Wikitext|Text|Freeform)$/) {
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
            if (ref($data->{Kids})) {
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

1;


=head1 FLAGS

RT::Tickets supports several flags which alter search behavior:


allow_deleted_search  (Otherwise never show deleted tickets in search results)
looking_at_type (otherwise limit to type=ticket)

These flags are set by calling

$tickets->{'flagname'} = 1;

BUG: There should be an API for this

=cut

=begin testing

# We assume that we've got some assets hanging around from before.
ok( my $unlimitassets = RTx::AssetTracker::Assets->new( $RT::SystemUser ) );
ok( $unlimitassets->UnLimit );
ok( $unlimitassets->Count > 0, "UnLimited assets object should return assets" );

=end testing


1;
