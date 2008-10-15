# BEGIN LICENSE BLOCK
# 
#  Copyright (c) 2002-2003 Jesse Vincent <jesse@bestpractical.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of version 2 of the GNU General Public License 
#  as published by the Free Software Foundation.
# 
#  A copy of that license should have arrived with this
#  software, but in any event can be snarfed from www.gnu.org.
# 
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
# END LICENSE BLOCK

=head1 NAME 

RTx::AssetTracker::System

=head1 DESCRIPTION

RTx::AssetTracker::System is a simple global object used as a focal point for things
that are system-wide.

It works sort of like an RT::Record, except it's really a single object that has
an id of "1" when instantiated.

This gets used by the ACL system so that you can have rights for the scope "RTx::AssetTracker::System"

In the future, there will probably be other API goodness encapsulated here.

=cut


package RTx::AssetTracker::System;
use RT::ACL;
use base qw /RT::Base/;
use strict;
use vars qw/ $RIGHTS/;


# Tell RT::ACE that this sort of object can get acls granted
$RT::ACE::OBJECT_TYPES{'RTx::AssetTracker::System'} = 1;


# System rights are rights granted to the whole system
# XXX TODO Can't localize these outside of having an object around.
$RIGHTS = {
	SuperUser => "Do anything with assets",     # loc_pair
	BulkUpdate => "Allow educated users to perform bulk updates",     # loc_pair
	#ShowConfigTab => "show Configuration tab",     # loc_pair
};


foreach my $right ( keys %{$RIGHTS} ) {
    $RT::ACE::LOWERCASERIGHTNAMES{ lc $right } = $right;
}


=head2 AvailableRights

Returns a hash of available rights for this object. The keys are the right names and the values are a description of what the rights do

=cut

sub AvailableRights {
    my $self = shift;

    my $type = RTx::AssetTracker::Type->new($RT::SystemUser);

    my $tr = $type->AvailableRights();

    # Build a merged list of all system wide rights, queue rights and group rights.
    my %rights = (%{$RIGHTS}, %{$tr},);
    return(\%rights);
}


=head2 new

Create a new RTx::AssetTracker::System object. Really, you should be using $RT::FM::System

=cut

                         
sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self  = {};
    bless( $self, $class );


    return ($self);
}

=head2 id

Returns RTx::AssetTracker::System's id. It's 1. 


=begin testing

use RTx::AssetTracker::System;
my $sys = RTx::AssetTracker::System->new();
is( $sys->Id, 1);
is ($sys->id, 1);

=end testing


=cut

*Id = \&id;

sub id {
    return (1);
}

=head2 Load

for compatibility. dummy method

=cut

sub Load {
    return(1);
}
1;
