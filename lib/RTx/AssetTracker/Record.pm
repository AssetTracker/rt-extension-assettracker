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

  RTx::AssetTracker::Record - Base class for AT record objects

=head1 SYNOPSIS


=head1 DESCRIPTION



=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Record;
use base 'RT::Record';

use RT::Link;


=head2 URI

Returns this record's URI

=cut

# Not sure why this is implemented, but need to override - Todd
sub URI {
    my $self = shift;
    my $uri = RT::URI::at->new($self->CurrentUser);
    return($uri->URIForObject($self));
}


=head2 Update  ARGSHASH

Updates fields on an object for you using the proper Set methods,
skipping unchanged values.

 ARGSRef => a hashref of attributes => value for the update
 AttributesRef => an arrayref of keys in ARGSRef that should be updated
 AttributePrefix => a prefix that should be added to the attributes in AttributesRef
                    when looking up values in ARGSRef
                    Bare attributes are tried before prefixed attributes

Returns a list of localized results of the update

=cut

sub Update {
    my $self = shift;

    my %args = (
        ARGSRef         => undef,
        AttributesRef   => undef,
        AttributePrefix => undef,
        @_
    );

    my $attributes = $args{'AttributesRef'};
    my $ARGSRef    = $args{'ARGSRef'};
    my %new_values;

    # gather all new values
    foreach my $attribute (@$attributes) {
        my $value;
        if ( defined $ARGSRef->{$attribute} ) {
            $value = $ARGSRef->{$attribute};
        }
        elsif (
            defined( $args{'AttributePrefix'} )
            && defined(
                $ARGSRef->{ $args{'AttributePrefix'} . "-" . $attribute }
            )
          ) {
            $value = $ARGSRef->{ $args{'AttributePrefix'} . "-" . $attribute };

        }
        else {
            next;
        }

        $value =~ s/\r\n/\n/gs;

        my $truncated_value = $self->TruncateValue($attribute, $value);

        # If Queue is 'General', we want to resolve the queue name for
        # the object.

        # This is in an eval block because $object might not exist.
        # and might not have a Name method. But "can" won't find autoloaded
        # items. If it fails, we don't care
        do {
            no warnings "uninitialized";
            local $@;
            eval {
                my $object = $attribute . "Obj";
                my $name = $self->$object->Name;
                next if $name eq $value || $name eq ($value || 0);
            };

            my $current = $self->$attribute();
            # RT::Queue->Lifecycle returns a Lifecycle object instead of name
            $current = eval { $current->Name } if ref $current;
            next if $truncated_value eq $current;
            next if ( $truncated_value || 0 ) eq $current;
        };

        $new_values{$attribute} = $value;
    }

    return $self->_UpdateAttributes(
        Attributes => $attributes,
        NewValues  => \%new_values,
        TransactionData => $ARGSRef->{BasicComment} || $ARGSRef->{GlobalComment},
    );
}

sub _UpdateAttributes {
    my $self = shift;
    my %args = (
        Attributes => [],
        NewValues  => {},
        TransactionData => undef,
        @_,
    );

    my @results;

    foreach my $attribute (@{ $args{Attributes} }) {
        next if !exists($args{NewValues}{$attribute});

        my $value = $args{NewValues}{$attribute};
        my $method = "Set$attribute";
        my ( $code, $msg );
        if (ref $self eq 'RTx::AssetTracker::Asset') {
            ( $code, $msg ) = $self->$method(Value => $value, TransactionData => $args{'TransactionData'});
        }
        else {
            ( $code, $msg ) = $self->$method($value);
        }
        my ($prefix) = ref($self) =~ /RT(?:.*)::(\w+)/;

        # Default to $id, but use name if we can get it.
        my $label = $self->id;
        $label = $self->Name if (UNIVERSAL::can($self,'Name'));
        # this requires model names to be loc'ed.

=for loc

    "Asset" # loc
    "Asset type" # loc

=cut

        push @results, $self->loc( $prefix ) . " $label: ". $msg;

=for loc

                                   "[_1] could not be set to [_2].",       # loc
                                   "That is already the current value",    # loc
                                   "No value sent to _Set!",               # loc
                                   "Illegal value for [_1]",               # loc
                                   "The new value has been set.",          # loc
                                   "No column specified",                  # loc
                                   "Immutable field",                      # loc
                                   "Nonexistant field?",                   # loc
                                   "Invalid data",                         # loc
                                   "Couldn't find row",                    # loc
                                   "Missing a primary key?: [_1]",         # loc
                                   "Found Object",                         # loc

=cut

    }

    return @results;
}


=head2 _AddLink

Takes a paramhash of Type and one of Base or Target. Adds that link to this object.

If Silent is true then no transactions will be recorded.  You can individually
control transactions on both base and target and with SilentBase and
SilentTarget respectively. By default both transactions are created.

Returns a tuple of (link ID, message, flag if link already existed).

=cut

sub _AddLink {
    my $self = shift;
    my %args = (
        Target       => '',
        Base         => '',
        Type         => '',
        Silent       => undef,
        Silent       => undef,
        SilentBase   => undef,
        SilentTarget => undef,
        TransactionData => undef,
        @_
    );

    # Remote_link is the URI of the object that is not this ticket
    my $remote_link;
    my $direction;

    if ( $args{'Base'} and $args{'Target'} ) {
        $RT::Logger->debug( "$self tried to create a link. both base and target were specified" );
        return ( 0, $self->loc("Can't specifiy both base and target") );
    }
    elsif ( $args{'Base'} ) {
        $args{'Target'} = $self->URI();
        $remote_link    = $args{'Base'};
        $direction      = 'Target';
    }
    elsif ( $args{'Target'} ) {
        $args{'Base'} = $self->URI();
        $remote_link  = $args{'Target'};
        $direction    = 'Base';
    }
    else {
        return ( 0, $self->loc('Either base or target must be specified') );
    }

    # Check if the link already exists - we don't want duplicates
    use RT::Link;
    my $old_link = RT::Link->new( $self->CurrentUser );
    $old_link->LoadByParams( Base   => $args{'Base'},
                             Type   => $args{'Type'},
                             Target => $args{'Target'} );
    if ( $old_link->Id ) {
        $RT::Logger->debug("$self Somebody tried to duplicate a link");
        return ( $old_link->id, $self->loc("Link already exists"), 1 );
    }

    # Storing the link in the DB.
    my $link = RT::Link->new( $self->CurrentUser );
    my ($linkid, $linkmsg) = $link->Create( Target => $args{Target},
                                            Base   => $args{Base},
                                            Type   => $args{Type} );

    unless ($linkid) {
        $RT::Logger->error("Link could not be created: ".$linkmsg);
        return ( 0, $self->loc("Link could not be created: [_1]", $linkmsg) );
    }

    my $basetext = $self->FormatLink(Object   => $link->BaseObj,
                                     FallBack => $args{Base});
    my $targettext = $self->FormatLink(Object   => $link->TargetObj,
                                       FallBack => $args{Target});
    my $typetext = $self->FormatType(Type => $args{Type});
    my $TransString = "$basetext $typetext $targettext.";

    # No transactions for you!
    return ($linkid, $TransString) if $args{'Silent'};

    # Some transactions?
    my $remote_uri = RT::URI->new( $self->CurrentUser );
    $remote_uri->FromURI( $remote_link );

    my $opposite_direction = $direction eq 'Target' ? 'Base': 'Target';

    unless ( $args{ 'Silent'. $direction } ) {
        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
            Type      => 'AddLink',
            Field     => $RT::Link::DIRMAP{$args{'Type'}}->{$direction},
            NewValue  => $remote_uri->URI || $remote_link,
            TimeTaken => 0,
	    Data      => $args{'TransactionData'},
        );
        $RT::Logger->error("Couldn't create transaction: $Msg") unless $Trans;
    }

    if ( !$args{"Silent$opposite_direction"} && $remote_uri->IsLocal ) {
        my $OtherObj = $remote_uri->Object;
        my ( $val, $msg ) = $OtherObj->_NewTransaction(
            Type           => 'AddLink',
            Field          => $RT::Link::DIRMAP{$args{'Type'}}->{$opposite_direction},
            NewValue       => $self->URI,
            ActivateScrips => !RT->Config->Get('LinkTransactionsRun1Scrip'),
            TimeTaken      => 0,
        );
        $RT::Logger->error("Couldn't create transaction: $msg") unless $val;
    }

    return ($linkid, $TransString);
}

=head2 _DeleteLink

Takes a paramhash of Type and one of Base or Target. Removes that link from this object.

If Silent is true then no transactions will be recorded.  You can individually
control transactions on both base and target and with SilentBase and
SilentTarget respectively. By default both transactions are created.

Returns a tuple of (status flag, message).

=cut 

sub _DeleteLink {
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

    # We want one of base and target. We don't care which but we only want _one_.
    my $direction;
    my $remote_link;

    if ( $args{'Base'} and $args{'Target'} ) {
        $RT::Logger->debug("$self ->_DeleteLink. got both Base and Target");
        return ( 0, $self->loc("Can't specifiy both base and target") );
    }
    elsif ( $args{'Base'} ) {
        $args{'Target'} = $self->URI();
        $remote_link    = $args{'Base'};
        $direction      = 'Target';
    }
    elsif ( $args{'Target'} ) {
        $args{'Base'} = $self->URI();
        $remote_link  = $args{'Target'};
        $direction    = 'Base';
    }
    else {
        $RT::Logger->error("Base or Target must be specified");
        return ( 0, $self->loc('Either base or target must be specified') );
    }

    my $link = RT::Link->new( $self->CurrentUser );
    $RT::Logger->debug( "Trying to load link: "
            . $args{'Base'} . " "
            . $args{'Type'} . " "
            . $args{'Target'} );

    $link->LoadByParams(
        Base   => $args{'Base'},
        Type   => $args{'Type'},
        Target => $args{'Target'}
    );

    unless ($link->id) {
        $RT::Logger->debug("Couldn't find that link");
        return ( 0, $self->loc("Link not found") );
    }

    my $basetext = $self->FormatLink(Object   => $link->BaseObj,
                                     FallBack => $args{Base});
    my $targettext = $self->FormatLink(Object   => $link->TargetObj,
                                       FallBack => $args{Target});
    my $typetext = $self->FormatType(Type => $args{Type});
    my $TransString = "$basetext no longer $typetext $targettext.";

    my ($ok, $msg) = $link->Delete();
    unless ($ok) {
        RT->Logger->error("Link could not be deleted: $msg");
        return ( 0, $self->loc("Link could not be deleted: [_1]", $msg) );
    }

    # No transactions for you!
    return (1, $TransString) if $args{'Silent'};

    # Some transactions?
    my $remote_uri = RT::URI->new( $self->CurrentUser );
    $remote_uri->FromURI( $remote_link );

    my $opposite_direction = $direction eq 'Target' ? 'Base': 'Target';

    unless ( $args{ 'Silent'. $direction } ) {
        my ( $Trans, $Msg, $TransObj ) = $self->_NewTransaction(
            Type      => 'DeleteLink',
            Field     => $RT::Link::DIRMAP{$args{'Type'}}->{$direction},
            OldValue  => $remote_uri->URI || $remote_link,
            TimeTaken => 0,
	    Data      => $args{'TransactionData'},
        );
        $RT::Logger->error("Couldn't create transaction: $Msg") unless $Trans;
    }

    if ( !$args{"Silent$opposite_direction"} && $remote_uri->IsLocal ) {
        my $OtherObj = $remote_uri->Object;
        my ( $val, $msg ) = $OtherObj->_NewTransaction(
            Type           => 'DeleteLink',
            Field          => $RT::Link::DIRMAP{$args{'Type'}}->{$opposite_direction},
            OldValue       => $self->URI,
            ActivateScrips => !RT->Config->Get('LinkTransactionsRun1Scrip'),
            TimeTaken      => 0,
        );
        $RT::Logger->error("Couldn't create transaction: $msg") unless $val;
    }

    return (1, $TransString);
}

# TODO: This _only_ works for RT::Foo classes. it doesn't work, for
# example, for RT::IR::Foo classes.

sub CustomFieldLookupId {
    my $self = shift;
    my $lookup = shift || $self->CustomFieldLookupType;
    my @classes = ($lookup =~ /RTx::AssetTracker::(\w+)-/g);

    # Work on "RT::Queue", for instance
    return $self->Id unless @classes;

    my $object = $self;
    # Save a ->Load call by not calling ->FooObj->Id, just ->Foo
    my $final = shift @classes;
    foreach my $class (reverse @classes) {
	my $method = "${class}Obj";
	$object = $object->$method;
    }

    my $id = $object->$final;
    unless (defined $id) {
        my $method = "${final}Obj";
        $id = $object->$method->Id;
    }
    return $id;
}


=item AddCustomFieldValue { Field => FIELD, Value => VALUE }

VALUE should be a string.
FIELD can be a CustomField object OR a CustomField ID.


Adds VALUE as a value of CustomField FIELD.  If this is a single-value custom field,
deletes the old value. 
If VALUE is not a valid value for the custom field, returns 
(0, 'Error message' ) otherwise, returns (1, 'Success Message')

=cut

# I guess I added this? -Todd
sub AddUniqueCustomFieldValue {

    my $self = shift;

    my %args = (
        Field             => undef,
        Value             => undef,
        @_
    );

    my $values = $self->CustomFieldValues($args{Field});

    while (my $value = $values->Next) {

        return(1, "$args{Value} is already a value.") if $args{Value} eq $value->Content;

    }

    $self->AddCustomFieldValue(@_);
}

# Can get rid of this after RT supports Data option. -Todd
sub _AddCustomFieldValue {
    my $self = shift;
    my %args = (
        Field             => undef,
        Value             => undef,
        LargeContent      => undef,
        ContentType       => undef,
        RecordTransaction => 1,
        Data              => undef,
        @_
    );

    my $cf = $self->LoadCustomFieldByIdentifier($args{'Field'});
    unless ( $cf->Id ) {
        return ( 0, $self->loc( "Custom field [_1] not found", $args{'Field'} ) );
    }

    my $OCFs = $self->CustomFields;
    $OCFs->Limit( FIELD => 'id', VALUE => $cf->Id );
    unless ( $OCFs->Count ) {
        return (
            0,
            $self->loc(
                "Custom field [_1] does not apply to this object",
                ref $args{'Field'} ? $args{'Field'}->id : $args{'Field'}
            )
        );
    }

    # empty string is not correct value of any CF, so undef it
    foreach ( qw(Value LargeContent) ) {
        $args{ $_ } = undef if defined $args{ $_ } && !length $args{ $_ };
    }

    unless ( $cf->ValidateValue( $args{'Value'} ) ) {
        return ( 0, $self->loc("Invalid value for custom field") );
    }

    # If the custom field only accepts a certain # of values, delete the existing
    # value and record a "changed from foo to bar" transaction
    unless ( $cf->UnlimitedValues ) {

        # Load up a ObjectCustomFieldValues object for this custom field and this ticket
        my $values = $cf->ValuesForObject($self);

        # We need to whack any old values here.  In most cases, the custom field should
        # only have one value to delete.  In the pathalogical case, this custom field
        # used to be a multiple and we have many values to whack....
        my $cf_values = $values->Count;

        if ( $cf_values > $cf->MaxValues ) {
            my $i = 0;   #We want to delete all but the max we can currently have , so we can then
                 # execute the same code to "change" the value from old to new
            while ( my $value = $values->Next ) {
                $i++;
                if ( $i < $cf_values ) {
                    my ( $val, $msg ) = $cf->DeleteValueForObject(
                        Object  => $self,
                        Content => $value->Content
                    );
                    unless ($val) {
                        return ( 0, $msg );
                    }
                    my ( $TransactionId, $Msg, $TransactionObj ) =
                      $self->_NewTransaction(
                        Type         => 'CustomField',
                        Field        => $cf->Id,
                        OldReference => $value,
                        Data         => $args{'Data'},
                      );
                }
            }
            $values->RedoSearch if $i; # redo search if have deleted at least one value
        }

        my ( $old_value, $old_content );
        if ( $old_value = $values->First ) {
            $old_content = $old_value->Content;
            $old_content = undef if defined $old_content && !length $old_content;

            my $is_the_same = 1;
            if ( defined $args{'Value'} ) {
                $is_the_same = 0 unless defined $old_content
                    && lc $old_content eq lc $args{'Value'};
            } else {
                $is_the_same = 0 if defined $old_content;
            }
            if ( $is_the_same ) {
                my $old_content = $old_value->LargeContent;
                if ( defined $args{'LargeContent'} ) {
                    $is_the_same = 0 unless defined $old_content
                        && $old_content eq $args{'LargeContent'};
                } else {
                    $is_the_same = 0 if defined $old_content;
                }
            }

            return $old_value->id if $is_the_same;
        }

        my ( $new_value_id, $value_msg ) = $cf->AddValueForObject(
            Object       => $self,
            Content      => $args{'Value'},
            LargeContent => $args{'LargeContent'},
            ContentType  => $args{'ContentType'},
        );

        unless ( $new_value_id ) {
            return ( 0, $self->loc( "Could not add new custom field value: [_1]", $value_msg ) );
        }

        my $new_value = RT::ObjectCustomFieldValue->new( $self->CurrentUser );
        $new_value->Load( $new_value_id );

        # now that adding the new value was successful, delete the old one
        if ( $old_value ) {
            my ( $val, $msg ) = $old_value->Delete();
            return ( 0, $msg ) unless $val;
        }

        if ( $args{'RecordTransaction'} ) {
            my ( $TransactionId, $Msg, $TransactionObj ) =
              $self->_NewTransaction(
                Type         => 'CustomField',
                Field        => $cf->Id,
                OldReference => $old_value,
                NewReference => $new_value,
                Data         => $args{'Data'},
              );
        }

        my $new_content = $new_value->Content;

        # For datetime, we need to display them in "human" format in result message
        #XXX TODO how about date without time?
        if ($cf->Type eq 'DateTime') {
            my $DateObj = RT::Date->new( $self->CurrentUser );
            $DateObj->Set(
                Format => 'ISO',
                Value  => $new_content,
            );
            $new_content = $DateObj->AsString;

            if ( defined $old_content && length $old_content ) {
                $DateObj->Set(
                    Format => 'ISO',
                    Value  => $old_content,
                );
                $old_content = $DateObj->AsString;
            }
        }

        unless ( defined $old_content && length $old_content ) {
            return ( $new_value_id, $self->loc( "[_1] [_2] added", $cf->Name, $new_content ));
        }
        elsif ( !defined $new_content || !length $new_content ) {
            return ( $new_value_id,
                $self->loc( "[_1] [_2] deleted", $cf->Name, $old_content ) );
        }
        else {
            return ( $new_value_id, $self->loc( "[_1] [_2] changed to [_3]", $cf->Name, $old_content, $new_content));
        }

    }

    # otherwise, just add a new value and record "new value added"
    else {
        my ($new_value_id, $msg) = $cf->AddValueForObject(
            Object       => $self,
            Content      => $args{'Value'},
            LargeContent => $args{'LargeContent'},
            ContentType  => $args{'ContentType'},
        );

        unless ( $new_value_id ) {
            return ( 0, $self->loc( "Could not add new custom field value: [_1]", $msg ) );
        }
        if ( $args{'RecordTransaction'} ) {
            my ( $tid, $msg ) = $self->_NewTransaction(
                Type          => 'CustomField',
                Field         => $cf->Id,
                NewReference  => $new_value_id,
                ReferenceType => 'RT::ObjectCustomFieldValue',
                Data         => $args{'Data'},
            );
            unless ( $tid ) {
                return ( 0, $self->loc( "Couldn't create a transaction: [_1]", $msg ) );
            }
        }
        return ( $new_value_id, $self->loc( "[_1] added as a value for [_2]", $args{'Value'}, $cf->Name ) );
    }
}



=head2 DeleteCustomFieldValue { Field => FIELD, Value => VALUE }

Deletes VALUE as a value of CustomField FIELD.

VALUE can be a string, a CustomFieldValue or a ObjectCustomFieldValue.

If VALUE is not a valid value for the custom field, returns
(0, 'Error message' ) otherwise, returns (1, 'Success Message')

=cut

# Need to submit patch to RT for Data option. -Todd
sub DeleteCustomFieldValue {
    my $self = shift;
    my %args = (
        Field   => undef,
        Value   => undef,
        ValueId => undef,
        Data    => undef,
        @_
    );

    my $cf = $self->LoadCustomFieldByIdentifier($args{'Field'});
    unless ( $cf->Id ) {
        return ( 0, $self->loc( "Custom field [_1] not found", $args{'Field'} ) );
    }

    my ( $val, $msg ) = $cf->DeleteValueForObject(
        Object  => $self,
        Id      => $args{'ValueId'},
        Content => $args{'Value'},
    );
    unless ($val) {
        return ( 0, $msg );
    }

    my ( $TransactionId, $Msg, $TransactionObj ) = $self->_NewTransaction(
        Type          => 'CustomField',
        Field         => $cf->Id,
        OldReference  => $val,
        ReferenceType => 'RT::ObjectCustomFieldValue',
        Data          => $args{'Data'},
    );
    unless ($TransactionId) {
        return ( 0, $self->loc( "Couldn't create a transaction: [_1]", $Msg ) );
    }

    my $old_value = $TransactionObj->OldValue;
    # For datetime, we need to display them in "human" format in result message
    if ( $cf->Type eq 'DateTime' ) {
        my $DateObj = RT::Date->new( $self->CurrentUser );
        $DateObj->Set(
            Format => 'ISO',
            Value  => $old_value,
        );
        $old_value = $DateObj->AsString;
    }
    return (
        $TransactionId,
        $self->loc(
            "[_1] is no longer a value for custom field [_2]",
            $old_value, $cf->Name
        )
    );
}


# Needed for asset selection when linking
sub BasicColumns {
    (
        [ Name => 'Name' ],
        [ Description => 'Description' ],
        [ Type => 'Type' ],
        [ Status => 'Status' ],
    );
}


=head2 UnresolvedDependencies

Returns an RT::Tickets object of tickets which this ticket depends on
and which have a status of new, open or stalled. (That list comes from
RT::Queue->ActiveStatusArray

=cut


# review -Todd
sub UnresolvedDependencies {
    my $self = shift;
    my $deps = RTx::AssetTracker::Assets->new($self->CurrentUser);

    my @live_statuses = RTx::AssetTracker::Type->ActiveStatusArray();
    foreach my $status (@live_statuses) {
        $deps->LimitStatus(VALUE => $status);
    }
    $deps->LimitDependedOnBy($self->Id);

    return($deps);

}


RT::Base->_ImportOverlays();

1;
