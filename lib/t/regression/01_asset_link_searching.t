#!/usr/bin/perl -w

use Test::More tests => 30;
use strict;
use RT;
use RTx::AssetTracker;
use RTx::AssetTracker::Type;
use RTx::AssetTracker::Assets;

# Load the config file
RT::LoadConfig();
RTx::AssetTracker::LoadConfig();

#Connect to the database and get RT::SystemUser and RT::Nobody loaded
RT::Init();

#Get the current user all loaded
my $CurrentUser = $RT::SystemUser;

my $type = new RTx::AssetTracker::Type($CurrentUser);
$type->Load('Servers') || Abort(loc("Type 'Servers' could not be loaded."));

my $child_asset = new RTx::AssetTracker::Asset( $CurrentUser );

my ( $childid ) = $child_asset->Create
    ( Name => "test child $$",
      Type => $type->Id);

ok($childid != 0, "Asset created");

my $parent_asset = new RTx::AssetTracker::Asset( $CurrentUser );

my ( $parentid ) = $parent_asset->Create
    ( Name => "test parent $$",
      Type => $type->Id);

ok($parentid != 0, "We created a parent asset");
my ($rv, $msg) = $parent_asset->AddLink(Base => $child_asset->URI, Type => 'ComponentOf');
ok($rv, "Relationship created");

my $Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->LimitComponentOf ($parent_asset->URI);

ok ($Collection->First);
is ($Collection->First->id, $childid, "We found the collection of all children of $parentid with Limit");
is($Collection->Count,1, "We found only one result");

$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL( "ComponentOf =  '" . $parent_asset->URI . "'");
is ($Collection->First->id, $childid, "We found the collection of all children of $parentid with AssetSQL");
is($Collection->Count,1, "We found only one result");





$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->LimitHasComponent ($child_asset->URI);

ok ($Collection->First);
is ($Collection->First->id, $parentid, "We found the collection of all parents of $childid with Limit");
is($Collection->Count,1, "We found only one result");



$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL("HasComponent = '" . $child_asset->URI . "'");

ok ($Collection->First);
is ($Collection->First->id, $parentid, "We found the collection of all parents of $childid with AssetSQL");
is($Collection->Count,1, "We found only one result");



# Now we find a collection of all the assets which have no components. they should have no children.
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->LimitHasComponent('');
# must contain child; must not contain parent
my %has;
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
ok ($has{$childid} , "The collection has our component - $childid");
ok( !$has{$parentid}, "The collection doesn't have our parent - $parentid");




# Now we find a collection of all the assets which are not components of anything. they should have no parents.
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->LimitComponentOf('');
# must contain parent; must not contain child
%has = ();
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
ok ($has{$parentid} , "The collection has our parent - $parentid");
ok( !$has{$childid}, "The collection doesn't have our component - $childid");


#  Do it all over with AssetSQL
#



# Now we find a collection of all the assets which have no components. they should have no children.
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL ("HasComponent IS NULL");
# must contain parent; must not contain child
 %has = ();
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
ok (!$has{$parentid} , "The collection doesn't have our parent - $parentid");
ok( $has{$childid}, "The collection has our component - $childid");


# Now we find a collection of all the assets which have no components. they should have no children.
# Alternate syntax
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL ("HasComponent = ''");
# must contain parent; must not contain child
 %has = ();
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
ok (!$has{$parentid} , "The collection doesn't have our parent - $parentid");
ok( $has{$childid}, "The collection has our component - $childid");



# Now we find a collection of all the assets which are not components of anything. they should have no parents.
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL("ComponentOf IS NULL");
# must not  contain parent; must contain parent
%has = ();
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
ok ($has{$parentid} , "The collection has our parent - $parentid");
ok(!$has{$childid}, "The collection doesn't have our component - $childid");


# Now we find a collection of all the assets which are not components of anything. they should have no parents.
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL("ComponentOf = ''");
# must not  contain parent; must contain parent
%has = ();
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
ok ($has{$parentid} , "The collection has our parent - $parentid");
ok(!$has{$childid}, "The collection doesn't have our component - $childid");

my $nolink_asset = new RTx::AssetTracker::Asset( $CurrentUser );

my ( $nolinkid ) = $nolink_asset->Create
    ( Name => "test nolink $$",
      Type => $type->Id);

ok($nolinkid != 0, "Asset created");

# Now we find a collection of all the assets which are not linked to anything.
$Collection = RTx::AssetTracker::Assets->new($CurrentUser);
$Collection->FromSQL("LinkedTo = ''");
%has = ();
while (my $t = $Collection->Next) {
    ++$has{$t->id};
}
TODO: {
local $TODO = 'Waiting for Jesse to apply my LinkedTo patch';
ok(!$has{$parentid} , "The collection doesn't have our parent - $parentid");
ok(!$has{$childid}, "The collection doesn't have our component - $childid");
ok( $has{$nolinkid}, "The collection doesn't have our component - $nolinkid");
}



1;


