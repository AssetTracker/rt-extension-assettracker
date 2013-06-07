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

use strict;


=head1 NAME

RTx::AssetTracker::IP


=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 METHODS

=cut

package RTx::AssetTracker::IP;
use RTx::AssetTracker::Record; 


use vars qw( @ISA );
@ISA= qw( RTx::AssetTracker::Record );

sub _Init {
  my $self = shift; 

  $self->Table('AT_IPs');
  $self->SUPER::_Init(@_);
}





=head2 Create PARAMHASH

Create takes a hash of values and creates a row in the database:

  varchar(15) 'IP'.
  varchar(12) 'MAC'.
  varchar(25) 'Interface'.
  int(11) 'Asset'.

=cut




sub Create {
    my $self = shift;
    my %args = ( 
                IP => '',
                MAC => '',
                Interface => '0',
                Asset => '0',

		  @_);
    $self->SUPER::Create(
                         IP => $args{'IP'},
                         MAC => $args{'MAC'},
                         Interface => $args{'Interface'},
                         Asset => $args{'Asset'},
);

}



=head2 id

Returns the current value of id. 
(In the database, id is stored as int(11).)


=cut


=head2 IP

Returns the current value of IP. 
(In the database, IP is stored as varchar(15).)



=head2 SetIP VALUE


Set IP to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, IP will be stored as a varchar(15).)


=cut


=head2 MAC

Returns the current value of MAC. 
(In the database, MAC is stored as varchar(12).)



=head2 SetMAC VALUE


Set MAC to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, MAC will be stored as a varchar(12).)


=cut


=head2 Interface

Returns the current value of Interface. 
(In the database, Interface is stored as varchar(25).)



=head2 SetInterface VALUE


Set Interface to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Interface will be stored as a varchar(25).)


=cut


=head2 Asset

Returns the current value of Asset. 
(In the database, Asset is stored as int(11).)



=head2 SetAsset VALUE


Set Asset to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Asset will be stored as a int(11).)


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



sub _CoreAccessible {
    {
     
        id =>
		{read => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => ''},
        IP => 
		{read => 1, write => 1, sql_type => 12, length => 15,  is_blob => 0,  is_numeric => 0,  type => 'varchar(15)', default => ''},
        MAC => 
		{read => 1, write => 1, sql_type => 12, length => 12,  is_blob => 0,  is_numeric => 0,  type => 'varchar(12)', default => ''},
        Interface => 
		{read => 1, write => 1, sql_type => 12, length => 25,  is_blob => 0,  is_numeric => 0,  type => 'varchar(25)', default => '0'},
        Asset => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Creator => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Created => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},
        LastUpdatedBy => 
		{read => 1, auto => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        LastUpdated => 
		{read => 1, auto => 1, sql_type => 11, length => 0,  is_blob => 0,  is_numeric => 0,  type => 'datetime', default => ''},

 }
};

RT::Base->_ImportOverlays();

1;
