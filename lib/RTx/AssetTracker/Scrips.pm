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

  RTx::AssetTracker::Scrips - a collection of RT Scrip objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Scrips;

=head1 DESCRIPTION


=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Scrips;
use base 'RTx::AssetTracker::SearchBuilder';

use RTx::AssetTracker::Scrip;

sub Table {'AT_Scrips'};


# {{{ sub LimitToAssetType 

=head2 LimitToAssetType

Takes a type id (numerical) as its only argument. Makes sure that 
Scopes it pulls out apply to this type (or another that you've selected with
another call to this method

=cut

sub LimitToAssetType  {
   my $self = shift;
  my $type = shift;
 
  $self->Limit(ENTRYAGGREGATOR => 'OR',
		                 FIELD => 'AssetType',
		                 VALUE => "$type") if defined $type;
  
}
# }}}

# {{{ sub LimitToGlobal

=head2 LimitToGlobal

Makes sure that 
Scopes it pulls out apply to all types (or another that you've selected with
another call to this method or LimitToAssetType

=cut


sub LimitToGlobal  {
   my $self = shift;
 
  $self->Limit (ENTRYAGGREGATOR => 'OR',
		FIELD => 'AssetType',
		VALUE => 0);
  
}
# }}}

# {{{ sub Next 

=head2 Next

Returns the next scrip that this user can see.

=cut
  
sub Next {
    my $self = shift;
    
    
    my $Scrip = $self->SUPER::Next();
    if ((defined($Scrip)) and (ref($Scrip))) {

	if ($Scrip->CurrentUserHasRight('ShowScrips')) {
	    return($Scrip);
	}
	
	#If the user doesn't have the right to show this scrip
	else {	
	    return($self->Next());
	}
    }
    #if there never was any scrip
    else {
	return(undef);
    }	
    
}
# }}}

=head2 Apply

Run through the relevant scrips.  Scrips will run in order based on 
description.  (Most common use case is to prepend a number to the description,
forcing the scrips to run in ascending alphanumerical order.)

=cut

sub Apply {
    my $self = shift;

    my %args = ( AssetObj      => undef,
                 Asset         => undef,
                 Transaction    => undef,
                 TransactionObj => undef,
                 Stage          => undef,
                 Type           => undef,
                 @_ );

    $self->Prepare(%args);
    $self->Commit();

}

=head2 Commit

Commit all of this object's prepared scrips

=cut

sub Commit {
    my $self = shift;

    
    foreach my $scrip (@{$self->Prepared}) {
        $RT::Logger->debug(
            "Committing scrip #". $scrip->id
            ." on txn #". $self->{'TransactionObj'}->id
            ." of asset #". $self->{'AssetObj'}->id
        );

        $scrip->Commit( AssetObj      => $self->{'AssetObj'},
                        TransactionObj => $self->{'TransactionObj'} );
    }
}


=head2 Prepare

Only prepare the scrips, returning an array of the scrips we're interested in
in order of preparation, not execution

=cut

sub Prepare { 
    my $self = shift;
    my %args = ( AssetObj      => undef,
                 Asset         => undef,
                 Transaction    => undef,
                 TransactionObj => undef,
                 Stage          => undef,
                 Type           => undef,
                 @_ );

    #We're really going to need a non-acled asset for the scrips to work
    $self->_SetupSourceObjects( AssetObj      => $args{'AssetObj'},
                                Asset         => $args{'Asset'},
                                TransactionObj => $args{'TransactionObj'},
                                Transaction    => $args{'Transaction'} );


    $self->_FindScrips( Stage => $args{'Stage'}, Type => $args{'Type'} );


    #Iterate through each script and check it's applicability.
    while ( my $scrip = $self->Next() ) {

          unless ( $scrip->IsApplicable(
                                     AssetObj      => $self->{'AssetObj'},
                                     TransactionObj => $self->{'TransactionObj'}
                   ) ) {
                   $RT::Logger->debug("Skipping Scrip #".$scrip->Id." because it isn't applicable");
                   next;
               }

        #If it's applicable, prepare and commit it
          unless ( $scrip->Prepare( AssetObj      => $self->{'AssetObj'},
                                    TransactionObj => $self->{'TransactionObj'}
                   ) ) {
                   $RT::Logger->debug("Skipping Scrip #".$scrip->Id." because it didn't Prepare");
                   next;
               }
        push @{$self->{'prepared_scrips'}}, $scrip;

    }

    return (@{$self->Prepared});

};

=head2 Prepared

Returns an arrayref of the scrips this object has prepared


=cut

sub Prepared {
    my $self = shift;
    return ($self->{'prepared_scrips'} || []);
}


# {{{ sup _SetupSourceObjects

=head2  _SetupSourceObjects { AssetObj , Asset, Transaction, TransactionObj }

Setup a asset and transaction for this Scrip collection to work with as it runs through the 
relevant scrips.  (Also to figure out which scrips apply)

Returns: nothing

=cut


sub _SetupSourceObjects {

    my $self = shift;
    my %args = ( 
            AssetObj => undef,
            Asset => undef,
            Transaction => undef,
            TransactionObj => undef,
            @_ );



    if ( $args{'AssetObj'} ) {
        # clone the asset here as we need to change CurrentUser
        $self->{'AssetObj'} = bless { %{$args{'AssetObj'} } }, 'RTx::AssetTracker::Asset';
        $self->{'AssetObj'}->CurrentUser( $self->CurrentUser );
    }
    else {
        $self->{'AssetObj'} = RTx::AssetTracker::Asset->new( $self->CurrentUser );
        $self->{'AssetObj'}->Load( $args{'Asset'} )
          || $RT::Logger->err("$self couldn't load asset $args{'Asset'}");
    }

    if ( ( $self->{'TransactionObj'} = $args{'TransactionObj'} ) ) {
        $self->{'TransactionObj'}->CurrentUser( $self->CurrentUser );
    }
    else {
        $self->{'TransactionObj'} = RT::Transaction->new( $self->CurrentUser );
        $self->{'TransactionObj'}->Load( $args{'Transaction'} )
          || $RT::Logger->err( "$self couldn't load transaction $args{'Transaction'}");
    }
} 

# }}}

# {{{ sub _FindScrips;

=head2 _FindScrips

Find only the apropriate scrips for whatever we're doing now.  Order them 
by their description.  (Most common use case is to prepend a number to the
description, forcing the scrips to display and run in ascending alphanumerical 
order.)

=cut

sub _FindScrips {
    my $self = shift;
    my %args = (
                 Stage => undef,
                 Type => undef,
                 @_ );


    $self->LimitToAssetType( $self->{'AssetObj'}->AssetTypeObj->Id );
      #Limit it to  $Asset->AssetTypeObj->Id
    $self->LimitToGlobal();
      # or to "global"

    $self->Limit( FIELD => "Stage", VALUE => $args{'Stage'} );

    my $ConditionsAlias = $self->NewAlias('AT_ScripConditions');

    $self->Join(
        ALIAS1 => 'main',
        FIELD1 => 'ScripCondition',
        ALIAS2 => $ConditionsAlias,
        FIELD2 => 'id'
    );

    #We only want things where the scrip applies to this sort of transaction
    # TransactionBatch stage can define list of transaction
    foreach( split /\s*,\s*/, ($args{'Type'} || '') ) {
	$self->Limit(
	    ALIAS           => $ConditionsAlias,
	    FIELD           => 'ApplicableTransTypes',
	    OPERATOR        => 'LIKE',
	    VALUE           => $_,
	    ENTRYAGGREGATOR => 'OR',
	)
    }

    # Or where the scrip applies to any transaction
    $self->Limit(
        ALIAS           => $ConditionsAlias,
        FIELD           => 'ApplicableTransTypes',
        OPERATOR        => 'LIKE',
        VALUE           => "Any",
        ENTRYAGGREGATOR => 'OR',
    );

    # Promise some kind of ordering
    $self->OrderBy( FIELD => 'Description' );

    # we call Count below, but later we always do search
    # so just do search and get count from results
    $self->_DoSearch if $self->{'must_redo_search'};

    $RT::Logger->debug(
        "Found ". $self->Count ." scrips for $args{'Stage'} stage"
        ." with applicable type(s) $args{'Type'}"
        ." for txn #".$self->{TransactionObj}->Id
        ." on asset #".$self->{AssetObj}->Id
    );
}

# }}}



=head2 NewItem

Returns an empty new RTx::AssetTracker::Scrip item

=cut

sub NewItem {
    my $self = shift;
    return(RTx::AssetTracker::Scrip->new($self->CurrentUser));
}

RT::Base->_ImportOverlays();

1;
