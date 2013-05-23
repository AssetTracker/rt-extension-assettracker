# BEGIN BPS TAGGED BLOCK {{{
# 
# COPYRIGHT:
# 
# This software is Copyright (c) 1996-2010 Best Practical Solutions, LLC
#                                          <jesse@bestpractical.com>
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

  RTx::AssetTracker::Scrip - an RT Scrip object

=head1 SYNOPSIS

  use RTx::AssetTracker::Scrip;

=head1 DESCRIPTION


=head1 METHODS


=cut


package RTx::AssetTracker::Scrip;

use strict;
no warnings qw(redefine);

# {{{ sub Create

=head2 Create

Creates a new entry in the Scrips table. Takes a paramhash with:

        AssetType                  => 0,
        Description            => undef,
        Template               => undef,
        ScripAction            => undef,
        ScripCondition         => undef,
        CustomPrepareCode      => undef,
        CustomCommitCode       => undef,
        CustomIsApplicableCode => undef,




Returns (retval, msg);
retval is 0 for failure or scrip id.  msg is a textual description of what happened.

=cut

sub Create {
    my $self = shift;
    my %args = (
        AssetType                  => 0,
        Template               => 0,                     # name or id
        ScripAction            => 0,                     # name or id
        ScripCondition         => 0,                     # name or id
        Stage                  => 'TransactionCreate',
        Description            => undef,
        CustomPrepareCode      => undef,
        CustomCommitCode       => undef,
        CustomIsApplicableCode => undef,
        @_
    );

    unless ( $args{'AssetType'} ) {
        unless ( $self->CurrentUser->HasRight( Object => $RTx::AssetTracker::System,
                                               Right  => 'ModifyScrips' ) )
        {
            return ( 0, $self->loc('Permission Denied') );
        }
        $args{'AssetType'} = 0;    # avoid undef sneaking in
    }
    else {
        my $AssetTypeObj = RTx::AssetTracker::Type->new( $self->CurrentUser );
        $AssetTypeObj->Load( $args{'AssetType'} );
        unless ( $AssetTypeObj->id ) {
            return ( 0, $self->loc('Invalid asset type') );
        }
        unless ( $AssetTypeObj->CurrentUserHasRight('ModifyScrips') ) {
            return ( 0, $self->loc('Permission Denied') );
        }
        $args{'AssetType'} = $AssetTypeObj->id;
    }

    #TODO +++ validate input

    require RTx::AssetTracker::ScripAction;
    return ( 0, $self->loc("Action is mandatory argument") )
        unless $args{'ScripAction'};
    my $action = RTx::AssetTracker::ScripAction->new( $self->CurrentUser );
    $action->Load( $args{'ScripAction'} );
    return ( 0, $self->loc( "Action '[_1]' not found", $args{'ScripAction'} ) ) 
        unless $action->Id;

    require RTx::AssetTracker::Template;
    return ( 0, $self->loc("Template is mandatory argument") )
        unless $args{'Template'};
    my $template = RTx::AssetTracker::Template->new( $self->CurrentUser );
    $template->Load( $args{'Template'} );
    return ( 0, $self->loc( "Template '[_1]' not found", $args{'Template'} ) )
        unless $template->Id;

    require RTx::AssetTracker::ScripCondition;
    return ( 0, $self->loc("Condition is mandatory argument") )
        unless $args{'ScripCondition'};
    my $condition = RTx::AssetTracker::ScripCondition->new( $self->CurrentUser );
    $condition->Load( $args{'ScripCondition'} );
    return ( 0, $self->loc( "Condition '[_1]' not found", $args{'ScripCondition'} ) )
        unless $condition->Id;

    my ( $id, $msg ) = $self->SUPER::Create(
        AssetType                  => $args{'AssetType'},
        Template               => $template->Id,
        ScripCondition         => $condition->id,
        Stage                  => $args{'Stage'},
        ScripAction            => $action->Id,
        Description            => $args{'Description'},
        CustomPrepareCode      => $args{'CustomPrepareCode'},
        CustomCommitCode       => $args{'CustomCommitCode'},
        CustomIsApplicableCode => $args{'CustomIsApplicableCode'},
    );
    if ( $id ) {
        return ( $id, $self->loc('Scrip Created') );
    }
    else {
        return ( $id, $msg );
    }
}

# }}}

# {{{ sub Delete

=head2 Delete

Delete this object

=cut

sub Delete {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('ModifyScrips') ) {
        return ( 0, $self->loc('Permission Denied') );
    }

    return ( $self->SUPER::Delete(@_) );
}

# }}}

# {{{ sub AssetTypeObj

=head2 AssetTypeObj

Retuns an RTx::AssetTracker::Type object with this Scrip\'s asset type

=cut

sub AssetTypeObj {
    my $self = shift;

    if ( !$self->{'AssetTypeObj'} ) {
        require RTx::AssetTracker::Type;
        $self->{'AssetTypeObj'} = RTx::AssetTracker::Type->new( $self->CurrentUser );
        $self->{'AssetTypeObj'}->Load( $self->__Value('AssetType') );
    }
    return ( $self->{'AssetTypeObj'} );
}

# }}}

# {{{ sub ActionObj

=head2 ActionObj

Retuns an RT::Action object with this Scrip\'s Action

=cut

sub ActionObj {
    my $self = shift;

    unless ( defined $self->{'ScripActionObj'} ) {
        require RTx::AssetTracker::ScripAction;

        $self->{'ScripActionObj'} = RTx::AssetTracker::ScripAction->new( $self->CurrentUser );

        #TODO: why are we loading Actions with templates like this.
        # two separate methods might make more sense
        $self->{'ScripActionObj'}->Load( $self->ScripAction, $self->Template );
    }
    return ( $self->{'ScripActionObj'} );
}

# }}}

# {{{ sub ConditionObj

=head2 ConditionObj

Retuns an L<RTx::AssetTracker::ScripCondition> object with this Scrip's IsApplicable

=cut

sub ConditionObj {
    my $self = shift;

    my $res = RTx::AssetTracker::ScripCondition->new( $self->CurrentUser );
    $res->Load( $self->ScripCondition );
    return $res;
}

# }}}

=head2 LoadModules

Loads scrip's condition and action modules.

=cut

sub LoadModules {
    my $self = shift;

    $self->ConditionObj->LoadCondition;
    $self->ActionObj->LoadAction;
}

# {{{ sub TemplateObj

=head2 TemplateObj

Retuns an RTx::AssetTracker::Template object with this Scrip\'s Template

=cut

sub TemplateObj {
    my $self = shift;

    unless ( defined $self->{'TemplateObj'} ) {
        require RTx::AssetTracker::Template;
        $self->{'TemplateObj'} = RTx::AssetTracker::Template->new( $self->CurrentUser );
        $self->{'TemplateObj'}->Load( $self->Template );
    }
    return ( $self->{'TemplateObj'} );
}

# }}}

# {{{ Dealing with this instance of a scrip

# {{{ sub Apply

=head2 Apply { AssetObj => undef, TransactionObj => undef}

This method instantiates the ScripCondition and ScripAction objects for a
single execution of this scrip. it then calls the IsApplicable method of the 
ScripCondition.
If that succeeds, it calls the Prepare method of the
ScripAction. If that succeeds, it calls the Commit method of the ScripAction.

Usually, the asset and transaction objects passed to this method
should be loaded by the SuperUser role

=cut


# XXX TODO : This code appears to be obsoleted in favor of similar code in Scrips->Apply.
# Why is this here? Is it still called?

sub Apply {
    my $self = shift;
    my %args = ( AssetObj      => undef,
                 TransactionObj => undef,
                 @_ );

    $RT::Logger->debug("Now applying scrip ".$self->Id . " for transaction ".$args{'TransactionObj'}->id);

    my $ApplicableTransactionObj = $self->IsApplicable( AssetObj      => $args{'AssetObj'},
                                                        TransactionObj => $args{'TransactionObj'} );
    unless ( $ApplicableTransactionObj ) {
        return undef;
    }

    if ( $ApplicableTransactionObj->id != $args{'TransactionObj'}->id ) {
        $RT::Logger->debug("Found an applicable transaction ".$ApplicableTransactionObj->Id . " in the same batch with transaction ".$args{'TransactionObj'}->id);
    }

    #If it's applicable, prepare and commit it
    $RT::Logger->debug("Now preparing scrip ".$self->Id . " for transaction ".$ApplicableTransactionObj->id);
    unless ( $self->Prepare( AssetObj      => $args{'AssetObj'},
                             TransactionObj => $ApplicableTransactionObj )
      ) {
        return undef;
    }

    $RT::Logger->debug("Now commiting scrip ".$self->Id . " for transaction ".$ApplicableTransactionObj->id);
    unless ( $self->Commit( AssetObj => $args{'AssetObj'},
                            TransactionObj => $ApplicableTransactionObj)
      ) {
        return undef;
    }

    $RT::Logger->debug("We actually finished scrip ".$self->Id . " for transaction ".$ApplicableTransactionObj->id);
    return (1);

}

# }}}

# {{{ sub IsApplicable

=head2 IsApplicable

Calls the  Condition object\'s IsApplicable method

Upon success, returns the applicable Transaction object.
Otherwise, undef is returned.

If the Scrip is in the TransactionCreate Stage (the usual case), only test
the associated Transaction object to see if it is applicable.

For Scrips in the TransactionBatch Stage, test all Transaction objects
created during the Asset object's lifetime, and returns the first one
that is applicable.

=cut

sub IsApplicable {
    my $self = shift;
    my %args = ( AssetObj      => undef,
                 TransactionObj => undef,
                 @_ );

    my $return;
    eval {

	my @Transactions;

        if ( $self->Stage eq 'TransactionCreate') {
	    # Only look at our current Transaction
	    @Transactions = ( $args{'TransactionObj'} );
        }
        elsif ( $self->Stage eq 'TransactionBatch') {
	    # Look at all Transactions in this Batch
            @Transactions = @{ $args{'AssetObj'}->TransactionBatch || [] };
        }
	else {
	    $RT::Logger->error( "Unknown Scrip stage:" . $self->Stage );
	    return (undef);
	}
	my $ConditionObj = $self->ConditionObj;
	foreach my $TransactionObj ( @Transactions ) {
	    # in TxnBatch stage we can select scrips that are not applicable to all txns
	    my $txn_type = $TransactionObj->Type;
	    next unless( $ConditionObj->ApplicableTransTypes =~ /(?:^|,)(?:Any|\Q$txn_type\E)(?:,|$)/i );
	    # Load the scrip's Condition object
	    $ConditionObj->LoadCondition(
		ScripObj       => $self,
		AssetObj      => $args{'AssetObj'},
		TransactionObj => $TransactionObj,
	    );

            if ( $ConditionObj->IsApplicable() ) {
	        # We found an application Transaction -- return it
                $return = $TransactionObj;
                last;
            }
	}
    };

    if ($@) {
        $RT::Logger->error( "Scrip IsApplicable " . $self->Id . " died. - " . $@ );
        return (undef);
    }

            return ($return);

}

# }}}

# {{{ SUb Prepare

=head2 Prepare

Calls the action object's prepare method

=cut

sub Prepare {
    my $self = shift;
    my %args = ( AssetObj      => undef,
                 TransactionObj => undef,
                 @_ );

    my $return;
    eval {
        $self->ActionObj->LoadAction( ScripObj       => $self,
                                      AssetObj      => $args{'AssetObj'},
                                      TransactionObj => $args{'TransactionObj'},
        );

        $return = $self->ActionObj->Prepare();
    };
    if ($@) {
        $RT::Logger->error( "Scrip Prepare " . $self->Id . " died. - " . $@ );
        return (undef);
    }
        unless ($return) {
        }
        return ($return);
}

# }}}

# {{{ sub Commit

=head2 Commit

Calls the action object's commit method

=cut

sub Commit {
    my $self = shift;
    my %args = ( AssetObj      => undef,
                 TransactionObj => undef,
                 @_ );

    my $return;
    eval {
        $return = $self->ActionObj->Commit();
    };

#Searchbuilder caching isn't perfectly coherent. got to reload the asset object, since it
# may have changed
    $args{'AssetObj'}->Load( $args{'AssetObj'}->Id );

    if ($@) {
        $RT::Logger->error( "Scrip Commit " . $self->Id . " died. - " . $@ );
        return (undef);
    }

    # Not destroying or weakening hte Action and Condition here could cause a
    # leak

    return ($return);
}

# }}}

# }}}

# {{{ ACL related methods

# {{{ sub _Set

# does an acl check and then passes off the call
sub _Set {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('ModifyScrips') ) {
        $RT::Logger->debug(
                 "CurrentUser can't modify Scrips for " . $self->AssetType . "\n" );
        return ( 0, $self->loc('Permission Denied') );
    }
    return $self->__Set(@_);
}

# }}}

# {{{ sub _Value
# does an acl check and then passes off the call
sub _Value {
    my $self = shift;

    unless ( $self->CurrentUserHasRight('ShowScrips') ) {
        $RT::Logger->debug( "CurrentUser can't modify Scrips for "
                            . $self->__Value('AssetType')
                            . "\n" );
        return (undef);
    }

    return $self->__Value(@_);
}

# }}}

# {{{ sub CurrentUserHasRight

=head2 CurrentUserHasRight

Helper menthod for HasRight. Presets Principal to CurrentUser then 
calls HasRight.

=cut

sub CurrentUserHasRight {
    my $self  = shift;
    my $right = shift;
    return ( $self->HasRight( Principal => $self->CurrentUser->UserObj,
                              Right     => $right ) );

}

# }}}

# {{{ sub HasRight

=head2 HasRight

Takes a param-hash consisting of "Right" and "Principal"  Principal is 
an RT::User object or an RT::CurrentUser object. "Right" is a textual
Right string that applies to Scrips.

=cut

sub HasRight {
    my $self = shift;
    my %args = ( Right     => undef,
                 Principal => undef,
                 @_ );

    if ( $self->SUPER::_Value('AssetType') ) {
        return $args{'Principal'}->HasRight(
            Right  => $args{'Right'},
            Object => $self->AssetTypeObj
        );
    }
    else {
        return $args{'Principal'}->HasRight(
            Object => $RTx::AssetTracker::System,
            Right  => $args{'Right'},
        );
    }
}

# }}}

# }}}


=head2 SetScripAction

=cut

sub SetScripAction {
    my $self  = shift;
    my $value = shift;

    return ( 0, $self->loc("Action is mandatory argument") ) unless $value;

    require RTx::AssetTracker::ScripAction;
    my $action = RTx::AssetTracker::ScripAction->new( $self->CurrentUser );
    $action->Load($value);
    return ( 0, $self->loc( "Action '[_1]' not found", $value ) )
      unless $action->Id;

    return $self->_Set( Field => 'ScripAction', Value => $action->Id );
}

=head2 SetScripCondition

=cut

sub SetScripCondition {
    my $self  = shift;
    my $value = shift;

    return ( 0, $self->loc("Condition is mandatory argument") )
      unless $value;

    require RTx::AssetTracker::ScripCondition;
    my $condition = RTx::AssetTracker::ScripCondition->new( $self->CurrentUser );
    $condition->Load($value);

    return ( 0, $self->loc( "Condition '[_1]' not found", $value ) )
      unless $condition->Id;

    return $self->_Set( Field => 'ScripCondition', Value => $condition->Id );
}

=head2 SetTemplate

=cut

sub SetTemplate {
    my $self  = shift;
    my $value = shift;

    return ( 0, $self->loc("Template is mandatory argument") ) unless $value;

    require RTx::AssetTracker::Template;
    my $template = RTx::AssetTracker::Template->new( $self->CurrentUser );
    $template->Load($value);
    return ( 0, $self->loc( "Template '[_1]' not found", $value ) )
      unless $template->Id;

    return $self->_Set( Field => 'Template', Value => $template->Id );
}

1;

