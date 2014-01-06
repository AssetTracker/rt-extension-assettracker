# BEGIN BPS TAGGED BLOCK {{{
#
# COPYRIGHT:
#
# This software is Copyright (c) 1996-2014 Best Practical Solutions, LLC
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

  RTx::AssetTracker::Templates - a collection of RT Template objects

=head1 SYNOPSIS

  use RTx::AssetTracker::Templates;

=head1 DESCRIPTION


=head1 METHODS


=cut

use strict;
use warnings;

package RTx::AssetTracker::Templates;
use base 'RT::SearchBuilder';

use RTx::AssetTracker::Template;

sub Table {'AT_Templates'};



=head2 _Init

  Returns RTx::AssetTracker::Templates specific init info like table and primary key names

=cut

sub _Init {
    
    my $self = shift;
    $self->{'table'} = "AT_Templates";
    $self->{'primary_key'} = "id";
    return ($self->SUPER::_Init(@_));
}


=head2 LimitToNotInAssetType

Takes an asset type id # and limits the returned set of templates to those which 
aren't thatn asset type's templates.

=cut

sub LimitToNotInAssetType {
    my $self = shift;
    my $assettype_id = shift;
    $self->Limit(FIELD => 'AssetType',
                 VALUE => "$assettype_id",
                 OPERATOR => '!='
                );
}


=head2 LimitToGlobal

Takes no arguments. Limits the returned set to "Global" templates
which can be used with any asset type.

=cut

sub LimitToGlobal {
    my $self = shift;
    $self->Limit(FIELD => 'AssetType',
                 VALUE => "0",
                 OPERATOR => '='
                );
}


=head2 LimitToAssetType

Takes an asset type id # and limits the returned set of templates to that asset type's
templates

=cut

sub LimitToAssetType {
    my $self = shift;
    my $assettype_id = shift;
    $self->Limit(FIELD => 'AssetType',
                 VALUE => "$assettype_id",
                 OPERATOR => '='
                );
}


=head2 Next

Returns the next template that this user can see.

=cut
  
sub Next {
    my $self = shift;
    
    
    my $templ = $self->SUPER::Next();
    if ((defined($templ)) and (ref($templ))) {
        
        # If it's part of an asset type, and the user can read templates in
        # that asset type, or the user can globally read templates, show it
        if ($templ->AssetType && $templ->CurrentUserHasAssetTypeRight('ShowTemplate') or
            $templ->CurrentUser->HasRight(Object => $RT::System, Right => 'ShowTemplate') or
            $templ->CurrentUser->HasRight(Object => $RT::System, Right => 'ShowGlobalTemplates')) {
	    return($templ);
	}
	
	#If the user doesn't have the right to show this template
	else {	
	    return($self->Next());
	}
    }
    #if there never was any template
    else {
	return(undef);
    }	
    
}


=head2 NewItem

Returns an empty new RTx::AssetTracker::Template item

=cut

sub NewItem {
    my $self = shift;
    return(RTx::AssetTracker::Template->new($self->CurrentUser));
}

RT::Base->_ImportOverlays();

1;
