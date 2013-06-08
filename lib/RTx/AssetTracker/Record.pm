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

  RTx::AssetTracker::Record - Base class for RT record objects

=head1 SYNOPSIS


=head1 DESCRIPTION



=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Record;
use base 'RT::Record';



# {{{ sub URI 

=head2 URI

Returns this record's URI

=cut

# Not sure why this is implemented, but need to override - Todd
sub URI {
    my $self = shift;
    my $uri = RT::URI::at->new($self->CurrentUser);
    return($uri->URIForObject($self));
}

# }}}

# Need to review this override -Todd
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
    my @results;

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


        # If Queue is 'General', we want to resolve the queue name for
        # the object.

        # This is in an eval block because $object might not exist.
        # and might not have a Name method. But "can" won't find autoloaded
        # items. If it fails, we don't care
        eval {
            my $object = $attribute . "Obj";
            next if ($self->$object->Name eq $value);
        };
        next if ( $value eq $self->$attribute() );
        my $method = "Set$attribute";
        my ( $code, $msg );
        if (ref $self eq 'RTx::AssetTracker::Asset') {
            ( $code, $msg ) = $self->$method(Value => $value, TransactionData => $ARGSRef->{BasicComment} || $ARGSRef->{GlobalComment});
        }
        else {
            ( $code, $msg ) = $self->$method($value);
        }

        my ($prefix) = ref($self) =~ /RTx::AssetTracker::(\w+)/;
        if ($prefix eq 'Asset') {
            push @results,
              $self->loc( "$prefix [_1] " . $self->Name, $self->id ) . ': '
              . $self->loc($attribute) . ': '
              . $self->CurrentUser->loc_fuzzy($msg);
        }
        else {
            push @results,
              $self->loc( "$prefix [_1]", $self->id ) . ': '
              . $self->loc($attribute) . ': '
              . $self->CurrentUser->loc_fuzzy($msg);
        }

=for loc
                                   "[_1] could not be set to [_2].",       # loc
                                   "That is already the current value",    # loc
                                   "No value sent to _Set!\n",             # loc
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

# {{{ Routines dealing with transactions

# {{{ sub _NewTransaction

=head2 _NewTransaction  PARAMHASH

Private function to create a new RT::Transaction object for this asset update

=cut

sub _NewTransaction {
    my $self = shift;
    my %args = (
        TimeTaken => undef,
        Type      => undef,
        OldValue  => undef,
        NewValue  => undef,
        OldReference  => undef,
        NewReference  => undef,
        ReferenceType => undef,
        Data      => undef,
        Field     => undef,
        MIMEObj   => undef,
        ActivateScrips => 1,
        CommitScrips => 1,
        @_
    );

    my $old_ref = $args{'OldReference'};
    my $new_ref = $args{'NewReference'};
    my $ref_type = $args{'ReferenceType'};
    if ($old_ref or $new_ref) {
	$ref_type ||= ref($old_ref) || ref($new_ref);
	if (!$ref_type) {
	    $RT::Logger->error("Reference type not specified for transaction");
	    return;
	}
	$old_ref = $old_ref->Id if ref($old_ref);
	$new_ref = $new_ref->Id if ref($new_ref);
    }

    require RT::Transaction;
    my $trans = new RT::Transaction( $self->CurrentUser );
    my ( $transaction, $msg ) = $trans->Create(
	ObjectId  => $self->Id,
	ObjectType => ref($self),
        TimeTaken => $args{'TimeTaken'},
        Type      => $args{'Type'},
        Data      => $args{'Data'},
        Field     => $args{'Field'},
        NewValue  => $args{'NewValue'},
        OldValue  => $args{'OldValue'},
        NewReference  => $new_ref,
        OldReference  => $old_ref,
        ReferenceType => $ref_type,
        MIMEObj   => $args{'MIMEObj'},
        ActivateScrips => $args{'ActivateScrips'},
        CommitScrips => $args{'CommitScrips'},
    );

    # Rationalize the object since we may have done things to it during the caching.
    $self->Load($self->Id);

    $RT::Logger->warning($msg) unless $transaction;

    $self->_SetLastUpdated;

    if ( defined $args{'TimeTaken'} and $self->can('_UpdateTimeTaken') ) {
        $self->_UpdateTimeTaken( $args{'TimeTaken'} );
    }
    if ( $RT::UseTransactionBatch and $transaction ) {
	    push @{$self->{_TransactionBatch}}, $trans;
    }
    return ( $transaction, $msg, $trans );
}

# }}}

# {{{ sub Transactions 

=head2 Transactions

  Returns an RT::Transactions object of all transactions on this record object

=cut

sub Transactions {
    my $self = shift;

    use RT::Transactions;
    my $transactions = RT::Transactions->new( $self->CurrentUser );

    #If the user has no rights, return an empty object
    $transactions->Limit(
        FIELD => 'ObjectId',
        VALUE => $self->id,
    );
    $transactions->Limit(
        FIELD => 'ObjectType',
        VALUE => ref($self),
    );

    return ($transactions);
}

# }}}
# }}}
#
# {{{ Routines dealing with custom fields

sub CustomFields {
    my $self = shift;
    my $cfs = RT::CustomFields->new( $self->CurrentUser );
    $cfs->UnLimit;

    # XXX handle multiple types properly
  if ($RT::VERSION gt '3.4.1' or $RT::VERSION eq '0.0.0' ) {
    $cfs->LimitToLookupType($self->CustomFieldLookupType);
    $cfs->LimitToGlobalOrObjectId($self->_LookupId($self->CustomFieldLookupType));
  } else {
    foreach my $lookup ($self->_LookupTypes) {
       $cfs->LimitToLookupType($lookup);
       $cfs->LimitToGlobalOrObjectId($self->_LookupId($lookup));
    }
  }


    return $cfs;
}

# TODO: This _only_ works for RT::Class classes. it doesn't work, for example, for RT::FM classes.

sub _LookupId {
    my $self = shift;
    my $lookup = shift;
    my @classes = ($lookup =~ /RTx::AssetTracker::(\w+)-/g);

    foreach my $class (reverse @classes) {
	my $method = "${class}Obj";
	$self = $self->$method;
    }

    return $self->Id;
}

# {{{ AddCustomFieldValue

=item AddCustomFieldValue { Field => FIELD, Value => VALUE }

VALUE should be a string.
FIELD can be a CustomField object OR a CustomField ID.


Adds VALUE as a value of CustomField FIELD.  If this is a single-value custom field,
deletes the old value. 
If VALUE isn't a valid value for the custom field, returns 
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
                $args{'Field'}
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

# }}}

# {{{ DeleteCustomFieldValue

=item DeleteCustomFieldValue { Field => FIELD, Value => VALUE }

Deletes VALUE as a value of CustomField FIELD.

VALUE can be a string, a CustomFieldValue or a ObjectCustomFieldValue.

If VALUE isn't a valid value for the custom field, returns
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

    my $cf = RT::CustomField->new( $self->CurrentUser );
    if ( UNIVERSAL::isa( $args{'Field'}, "RT::CustomField" ) ) {
        $cf->LoadById( $args{'Field'}->id );
    }
    else {
        $cf->LoadById( $args{'Field'} );
    }

    unless ( $cf->Id ) {
        return ( 0, $self->loc("Custom field not found") );
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

    return (
        $TransactionId,
        $self->loc(
            "[_1] is no longer a value for custom field [_2]",
            $TransactionObj->OldValue, $cf->Name
        )
    );
}

# }}}



# {{{ CustomFieldValues

=item CustomFieldValues FIELD

Return a ObjectCustomFieldValues object of all values of the CustomField whose id is FIELD for this asset.  

Returns an RT::ObjectCustomFieldValues object

=cut

sub CustomFieldValues {
    my $self  = shift;
    my $field = shift;

    my $cf_values = RT::ObjectCustomFieldValues->new( $self->CurrentUser );

    # If we've been handed a value that contains at least one non-digit,
    # it's a name.  Resolve it into an id.
    #
    if ( defined $field && $field =~ /\D+/ ) {

        # Look up the field ID.
        my $cfs = RT::CustomFields->new( $self->CurrentUser );
        $cfs->LimitToGlobalOrObjectId( $self->Id() );
        $cfs->LimitToLookupType($self->CustomFieldLookupType);
        $cfs->Limit( FIELD => 'Name', OPERATOR => '=', VALUE => $field );

        if ( $cfs->First ) {
            $field = $cfs->First->id;
        }
        else {

            #We didn't find a custom field, but they wanted one. let's
            # return an empty
            return ($cf_values);
        }
    }

    # If we now have a custom field id, let's limit things down
    # If we don't have a custom field ID, the $cf_values object will return
    #  all values
    $cf_values->LimitToCustomField($field) if ($field);
    $cf_values->LimitToObject($self);
    $cf_values->OrderBy( FIELD => 'id', ORDER => 'ASC' );


    return ($cf_values);
}

# }}}

# }}}

# }}}

# Do I need this? -Todd
sub BasicColumns {
    (
        [ Name => 'Name' ],
        [ Description => 'Description' ],
        [ Type => 'Type' ],
        [ Status => 'Status' ],
    );
}

# {{{ Routines dealing with Links

# {{{ Link Collections


# {{{ UnresolvedDependencies

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

# }}}

# }}}


# }}}

RT::Base->_ImportOverlays();

1;
