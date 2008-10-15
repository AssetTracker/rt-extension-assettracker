#!/usr/bin/perl

use Test::More tests => 9;
#use Test::More qw(no_plan);

use RT;
use RTx::AssetTracker;
ok(RT::LoadConfig);
ok(RTx::AssetTracker::LoadConfig);
ok(RT::Init, "Basic initialization and DB connectivity");

# Create a new type
use_ok(RTx::AssetTracker::Type);
my $t = RTx::AssetTracker::Type->new($RT::SystemUser);

$t->Load('regression');
if ($t->id != 0) {
        die "Regression tests not starting with a clean DB. Bailing";
}

my ($id, $msg) = $t->Create( Name => 'Regression',
            Description => 'A regression test type',
            );

isnt($id, 0, "Type was created sucessfully - $msg");

my $t2 = RTx::AssetTracker::Type->new($RT::SystemUser);

ok($t2->Load($id));
is($t2->id, $id, "Sucessfully loaded the type again");
is($t2->Name, 'Regression');
is($t2->Description, 'A regression test type');


