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

package RTx::AssetTracker;
use strict;
use warnings;
use version; our $VERSION = version->declare("2.9.0");


use RT::System;
RT::System::AddRights(
    BulkUpdate  => "Perform bulk updates on assets", # loc_pair
    AssetImport => "Import assets", # loc_pair
);

RT::System::AddRightCategories(
    BulkUpdate  => 'Staff',
    AssetImport => 'Staff',
);


# load overlays for RT classes
my @Classes = qw(
    RT::CustomField
    RT::CustomFields
    RT::Interface::Web
    RT::Interface::Web::QueryBuilder::Tree
    RT::Ticket
    RT::Transaction
);

for (@Classes) {
    s|::|/|g;
    require $_.'.pm';
    require $_.'_ATOverlay.pm';
}


use RTx::AssetTracker::Types;
use RTx::AssetTracker::Assets;
use RTx::AssetTracker::Templates;
use RTx::AssetTracker::Scrips;
use RTx::AssetTracker::ScripConditions;
use RTx::AssetTracker::ScripActions;

RTx::AssetTracker::Type->ConfigureRoles();
RTx::AssetTracker::Asset->ConfigureLinks();

1;
