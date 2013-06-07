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

use strict;


=head1 NAME

RTx::AssetTracker::Scrip


=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 METHODS

=cut

package RTx::AssetTracker::Scrip;
use RTx::AssetTracker::Record; 
use RTx::AssetTracker::Type;
use RTx::AssetTracker::Template;
use RTx::AssetTracker::ScripCondition;
use RTx::AssetTracker::ScripAction;


use vars qw( @ISA );
@ISA= qw( RTx::AssetTracker::Record );

sub _Init {
  my $self = shift; 

  $self->Table('AT_Scrips');
  $self->SUPER::_Init(@_);
}





=head2 Create PARAMHASH

Create takes a hash of values and creates a row in the database:

  varchar(255) 'Description'.
  int(11) 'ScripCondition'.
  int(11) 'ScripAction'.
  text 'ConditionRules'.
  text 'ActionRules'.
  text 'CustomIsApplicableCode'.
  text 'CustomPrepareCode'.
  text 'CustomCommitCode'.
  varchar(32) 'Stage'.
  int(11) 'AssetType'.
  int(11) 'Template'.

=cut




sub Create {
    my $self = shift;
    my %args = ( 
                Description => '',
                ScripCondition => '0',
                ScripAction => '0',
                ConditionRules => '',
                ActionRules => '',
                CustomIsApplicableCode => '',
                CustomPrepareCode => '',
                CustomCommitCode => '',
                Stage => '',
                AssetType => '0',
                Template => '0',

		  @_);
    $self->SUPER::Create(
                         Description => $args{'Description'},
                         ScripCondition => $args{'ScripCondition'},
                         ScripAction => $args{'ScripAction'},
                         ConditionRules => $args{'ConditionRules'},
                         ActionRules => $args{'ActionRules'},
                         CustomIsApplicableCode => $args{'CustomIsApplicableCode'},
                         CustomPrepareCode => $args{'CustomPrepareCode'},
                         CustomCommitCode => $args{'CustomCommitCode'},
                         Stage => $args{'Stage'},
                         AssetType => $args{'AssetType'},
                         Template => $args{'Template'},
);

}



=head2 id

Returns the current value of id. 
(In the database, id is stored as int(11).)


=cut


=head2 Description

Returns the current value of Description. 
(In the database, Description is stored as varchar(255).)



=head2 SetDescription VALUE


Set Description to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Description will be stored as a varchar(255).)


=cut


=head2 ScripCondition

Returns the current value of ScripCondition. 
(In the database, ScripCondition is stored as int(11).)



=head2 SetScripCondition VALUE


Set ScripCondition to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, ScripCondition will be stored as a int(11).)


=cut


=head2 ScripConditionObj

Returns the ScripCondition Object which has the id returned by ScripCondition


=cut

sub ScripConditionObj {
	my $self = shift;
	my $ScripCondition =  RTx::AssetTracker::ScripCondition->new($self->CurrentUser);
	$ScripCondition->Load($self->__Value('ScripCondition'));
	return($ScripCondition);
}

=head2 ScripAction

Returns the current value of ScripAction. 
(In the database, ScripAction is stored as int(11).)



=head2 SetScripAction VALUE


Set ScripAction to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, ScripAction will be stored as a int(11).)


=cut


=head2 ScripActionObj

Returns the ScripAction Object which has the id returned by ScripAction


=cut

sub ScripActionObj {
	my $self = shift;
	my $ScripAction =  RTx::AssetTracker::ScripAction->new($self->CurrentUser);
	$ScripAction->Load($self->__Value('ScripAction'));
	return($ScripAction);
}

=head2 ConditionRules

Returns the current value of ConditionRules. 
(In the database, ConditionRules is stored as text.)



=head2 SetConditionRules VALUE


Set ConditionRules to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, ConditionRules will be stored as a text.)


=cut


=head2 ActionRules

Returns the current value of ActionRules. 
(In the database, ActionRules is stored as text.)



=head2 SetActionRules VALUE


Set ActionRules to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, ActionRules will be stored as a text.)


=cut


=head2 CustomIsApplicableCode

Returns the current value of CustomIsApplicableCode. 
(In the database, CustomIsApplicableCode is stored as text.)



=head2 SetCustomIsApplicableCode VALUE


Set CustomIsApplicableCode to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, CustomIsApplicableCode will be stored as a text.)


=cut


=head2 CustomPrepareCode

Returns the current value of CustomPrepareCode. 
(In the database, CustomPrepareCode is stored as text.)



=head2 SetCustomPrepareCode VALUE


Set CustomPrepareCode to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, CustomPrepareCode will be stored as a text.)


=cut


=head2 CustomCommitCode

Returns the current value of CustomCommitCode. 
(In the database, CustomCommitCode is stored as text.)



=head2 SetCustomCommitCode VALUE


Set CustomCommitCode to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, CustomCommitCode will be stored as a text.)


=cut


=head2 Stage

Returns the current value of Stage. 
(In the database, Stage is stored as varchar(32).)



=head2 SetStage VALUE


Set Stage to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Stage will be stored as a varchar(32).)


=cut


=head2 AssetType

Returns the current value of AssetType. 
(In the database, AssetType is stored as int(11).)



=head2 SetAssetType VALUE


Set AssetType to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, AssetType will be stored as a int(11).)


=cut


=head2 AssetTypeObj

Returns the AssetType Object which has the id returned by AssetType


=cut

sub AssetTypeObj {
	my $self = shift;
	my $Type =  RTx::AssetTracker::Type->new($self->CurrentUser);
	$Type->Load($self->__Value('AssetType'));
	return($Type);
}

=head2 Template

Returns the current value of Template. 
(In the database, Template is stored as int(11).)



=head2 SetTemplate VALUE


Set Template to VALUE. 
Returns (1, 'Status message') on success and (0, 'Error Message') on failure.
(In the database, Template will be stored as a int(11).)


=cut


=head2 TemplateObj

Returns the Template Object which has the id returned by Template


=cut

sub TemplateObj {
	my $self = shift;
	my $Template =  RTx::AssetTracker::Template->new($self->CurrentUser);
	$Template->Load($self->__Value('Template'));
	return($Template);
}

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
        Description => 
		{read => 1, write => 1, sql_type => 12, length => 255,  is_blob => 0,  is_numeric => 0,  type => 'varchar(255)', default => ''},
        ScripCondition => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        ScripAction => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        ConditionRules => 
		{read => 1, write => 1, sql_type => -4, length => 0,  is_blob => 1,  is_numeric => 0,  type => 'text', default => ''},
        ActionRules => 
		{read => 1, write => 1, sql_type => -4, length => 0,  is_blob => 1,  is_numeric => 0,  type => 'text', default => ''},
        CustomIsApplicableCode => 
		{read => 1, write => 1, sql_type => -4, length => 0,  is_blob => 1,  is_numeric => 0,  type => 'text', default => ''},
        CustomPrepareCode => 
		{read => 1, write => 1, sql_type => -4, length => 0,  is_blob => 1,  is_numeric => 0,  type => 'text', default => ''},
        CustomCommitCode => 
		{read => 1, write => 1, sql_type => -4, length => 0,  is_blob => 1,  is_numeric => 0,  type => 'text', default => ''},
        Stage => 
		{read => 1, write => 1, sql_type => 12, length => 32,  is_blob => 0,  is_numeric => 0,  type => 'varchar(32)', default => ''},
        AssetType => 
		{read => 1, write => 1, sql_type => 4, length => 11,  is_blob => 0,  is_numeric => 1,  type => 'int(11)', default => '0'},
        Template => 
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
