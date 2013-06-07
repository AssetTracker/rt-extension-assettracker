# BEGIN BPS TAGGED BLOCK {{{
# 
# COPYRIGHT:
#  
# This software is Copyright (c) 1996-2004 Best Practical Solutions, LLC 
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
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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

  RTx::AssetTracker::Types - a collection of AssetTracker Types objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Types;

=head1 DESCRIPTION


=head1 METHODS

=cut

use strict;
use warnings;

package RTx::AssetTracker::Types;
use base 'RTx::AssetTracker::SearchBuilder';

use RTx::AssetTracker::Type;

sub Table {'AT_Types'};


# {{{ sub _Init
sub _Init {
  my $self = shift;
  $self->{'table'} = "AT_Types";
  $self->{'primary_key'} = "id";

  # By default, order by name
  $self->OrderBy( ALIAS => 'main',
                  FIELD => 'Name',
                  ORDER => 'ASC');

  return ($self->SUPER::_Init(@_));
}
# }}}

# {{{ sub _DoSearch

=head2 _DoSearch

  A subclass of DBIx::SearchBuilder::_DoSearch that makes sure that _Disabled rows never get seen unless
we're explicitly trying to see them.

=cut

sub _DoSearch {
    my $self = shift;

    #unless we really want to find disabled rows, make sure we\'re only finding enabled ones.
    unless($self->{'find_disabled_rows'}) {
        $self->LimitToEnabled();
    }

    return($self->SUPER::_DoSearch(@_));

}

# }}}


# {{{ sub Limit
sub Limit  {
  my $self = shift;
  my %args = ( ENTRYAGGREGATOR => 'AND',
               @_);
  $self->SUPER::Limit(%args);
}
# }}}

# {{{ sub Next

=head2 Next

Returns the next type that this user can see.

=cut

sub Next {
    my $self = shift;


    my $Type = $self->SUPER::Next();
    if ((defined($Type)) and (ref($Type))) {

        if ($Type->CurrentUserHasRight('SeeType')) {
            return($Type);
        }

        #If the user doesn't have the right to show this type
        else {
            return($self->Next());
        }
    }
    #if there never was any type
    else {
        return(undef);
    }

}
# }}}


=head2 NewItem

Returns an empty new RTx::AssetTracker::Type item

=cut

sub NewItem {
    my $self = shift;
    return(RTx::AssetTracker::Type->new($self->CurrentUser));
}

RT::Base->_ImportOverlays();

1;
