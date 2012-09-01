
use RTx::AssetTracker::Test nodata => 1, tests => 267;

use strict;
use warnings;


my $type_a = RTx::AssetTracker::Test->load_or_create_type( Name => 'A' );
ok $type_a && $type_a->id, 'loaded or created type_a';
my $qa_id = $type_a->id;

my $type_b = RTx::AssetTracker::Test->load_or_create_type( Name => 'B' );
ok $type_b && $type_b->id, 'loaded or created type_b';
my $qb_id = $type_b->id;

my $user_a = RTx::AssetTracker::Test->load_or_create_user(
    Name => 'user_a', Password => 'password',
);
ok $user_a && $user_a->id, 'loaded or created user';

my $user_b = RTx::AssetTracker::Test->load_or_create_user(
    Name => 'user_b', Password => 'password',
);
ok $user_b && $user_b->id, 'loaded or created user';

foreach my $option (0 .. 1 ) { RT->Config->Set( 'UseSQLForACLChecks' => $option );

diag "Testing with UseSQLForACLChecks => $option";

# Global Admin has right, a User is nobody
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Admin',    Right => [qw(ShowAsset)] },
    );
    create_assets_set();
    have_no_rights($user_a, $user_b);
}

# Global Admin has right, a User is Type Admin
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Admin',    Right => [qw(ShowAsset)] },
    );
    create_assets_set();
    have_no_rights($user_a, $user_b);

    my ($status, $msg) = $type_a->AddWatcher( Type => 'Admin', PrincipalId => $user_a->id );
    ok($status, "user A is now type A watcher");

    foreach my $q (
        '',
        "Type = $qa_id OR Type = $qb_id",
        "Type = $qb_id OR Type = $qa_id",
    ) {
        my $assets = RTx::AssetTracker::Assets->new( RT::CurrentUser->new( $user_a ) );
        $q? $assets->FromSQL($q) : $assets->UnLimit;
        my $found = 0;
        while ( my $t = $assets->Next ) {
            $found++;
            is( $t->Type, $type_a->id, "user sees assets only of type A" );
        }
        is($found, 2, "user sees assets");
    }
    have_no_rights( $user_b );
}

# global Admin has right, a User is asset Admin
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Admin',    Right => [qw(ShowAsset)] },
    );
    my @assets = create_assets_set();
    have_no_rights($user_a, $user_b);

    my ($status, $msg) = $assets[1]->AddWatcher( Type => 'Admin', PrincipalId => $user_a->id );
    ok($status, "user A is now type A watcher");

    foreach my $q (
        '',
        "Type = $qa_id OR Type = $qb_id",
        "Type = $qb_id OR Type = $qa_id",
    ) {
        my $assets = RTx::AssetTracker::Assets->new( RT::CurrentUser->new( $user_a ) );
        $q? $assets->FromSQL($q) : $assets->UnLimit;
        my $found = 0;
        while ( my $t = $assets->Next ) {
            $found++;
            is( $t->Type, $type_a->id, "user sees assets only in type A" );
            is( $t->id, $assets[1]->id, "correct asset");
        }
        is($found, 1, "user sees assets");
    }
    have_no_rights($user_b);
}

# Type Admin has right, a User is nobody
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Admin', Object => $type_a, Right => [qw(ShowAsset)] },
    );
    create_assets_set();
    RT::Logger->info("START");
    have_no_rights($user_a, $user_b);
    RT::Logger->info("END");
}

# Type Admin has right, Users are Type Admins
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Admin', Object => $type_a, Right => [qw(ShowAsset)] },
    );
    create_assets_set();
    have_no_rights($user_a, $user_b);

    my ($status, $msg) = $type_a->AddWatcher( Type => 'Admin', PrincipalId => $user_a->id );
    ok($status, "user A is now type A watcher");

    ($status, $msg) = $type_b->AddWatcher( Type => 'Admin', PrincipalId => $user_b->id );
    ok($status, "user B is now type B watcher");

    foreach my $q (
        '',
        "Type = $qa_id OR Type = $qb_id",
        "Type = $qb_id OR Type = $qa_id",
    ) {
        my $assets = RTx::AssetTracker::Assets->new( RT::CurrentUser->new( $user_a ) );
        $q? $assets->FromSQL($q) : $assets->UnLimit;
        my $found = 0;
        while ( my $t = $assets->Next ) {
            $found++;
            is( $t->Type, $type_a->id, "user sees assets only in type A" );
        }
        is($found, 2, "user sees assets");
    }
    have_no_rights( $user_b );
}

# Type Admin has right, Users are asset Admins
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Admin', Object => $type_a, Right => [qw(ShowAsset)] },
    );
    my @assets = create_assets_set();
    have_no_rights($user_a, $user_b);

    my ($status, $msg) = $assets[1]->AddWatcher( Type => 'Admin', PrincipalId => $user_a->id );
    ok($status, "user A is now Admin on a asset in type A");

    ($status, $msg) = $assets[2]->AddWatcher( Type => 'Admin', PrincipalId => $user_b->id );
    ok($status, "user B is now Admin on a asset in type B");

    foreach my $q (
        '',
        "Type = $qa_id OR Type = $qb_id",
        "Type = $qb_id OR Type = $qa_id",
    ) {
        my $assets = RTx::AssetTracker::Assets->new( RT::CurrentUser->new( $user_a ) );
        $q? $assets->FromSQL($q) : $assets->UnLimit;
        my $found = 0;
        while ( my $t = $assets->Next ) {
            $found++;
            is( $t->Type, $type_a->id, "user sees assets only in type A" );
            is( $t->id, $assets[1]->id, )
        }
        is($found, 1, "user sees assets");
    }
    have_no_rights( $user_b );
}

# Users has direct right on type
{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => $user_a, Object => $type_a, Right => [qw(ShowAsset)] },
    );
    my @assets = create_assets_set();

    foreach my $q (
        '',
        "Type = $qa_id OR Type = $qb_id",
        "Type = $qb_id OR Type = $qa_id",
    ) {
        my $assets = RTx::AssetTracker::Assets->new( RT::CurrentUser->new( $user_a ) );
        $q? $assets->FromSQL($q) : $assets->UnLimit;
        my $found = 0;
        while ( my $t = $assets->Next ) {
            $found++;
            is( $t->Type, $type_a->id, "user sees assets only in type A" );
        }
        is($found, 2, "user sees assets");
    }
    have_no_rights( $user_b );
}


}

sub have_no_rights {
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    foreach my $u ( @_ ) {
        foreach my $q (
            '',
            "Type = $qa_id OR Type = $qb_id",
            "Type = $qb_id OR Type = $qa_id",
        ) {
            my $assets = RTx::AssetTracker::Assets->new( RT::CurrentUser->new( $u ) );
            $q? $assets->FromSQL($q) : $assets->UnLimit;
            ok(!$assets->First, "no assets");
        }
    }
}

sub create_assets_set{
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my @res;
    foreach my $q ($type_a, $type_b) {
        foreach my $n (1 .. 2) {
            my $asset = RTx::AssetTracker::Asset->new( RT->SystemUser );
            my ($tid) = $asset->Create(
                Type => $q->id, Name => $q->Name .' - '. $n
            );
            ok( $tid, "created asset #$tid");
            push @res, $asset;
        }
    }
    return @res;
}

sub cleanup {
    RTx::AssetTracker::Test->delete_assets( "Type = $qa_id OR Type = $qb_id" );
    RTx::AssetTracker::Test->delete_type_watchers( $type_a, $type_b );
};
