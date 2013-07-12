use RTx::AssetTracker::Test tests => 14;

use strict;
use warnings;

my $type = RTx::AssetTracker::Test->load_or_create_type( Name => 'A' );
ok $type && $type->id, 'loaded or created type_a';
my $qid = $type->id;

my $user = RTx::AssetTracker::Test->load_or_create_user(
    Name => 'user',
    Password => 'password',
    EmailAddress => 'test@example.com',
);
ok $user && $user->id, 'loaded or created user';

{
    cleanup();
    RTx::AssetTracker::Test->set_rights(
        { Principal => 'Everyone', Right => [qw(SeeType)] },
        { Principal => 'Owner',    Right => [qw(ShowAsset)] },
    );
    my ($a) = RTx::AssetTracker::Test->create_assets(
        { Type => $type->id },
        { },
    );
    my $rights;
    if ( $RT::VERSION =~ /^3/ ) {
        ok( $user->HasRight( Object => $a, Right => 'SeeType' ) && !$user->HasRight( Object => $a, Right => 'ShowAsset' ), 'got it' );
    } else {
        $rights = $user->PrincipalObj->HasRights( Object => $a );
        is_deeply( $rights, { SeeType => 1 }, 'got it' );
    }

    cleanup();
    ($a) = RTx::AssetTracker::Test->create_assets(
        { Type => $type->id },
        { Owner => $user->EmailAddress },
    );
    ok($a->OwnerRoleGroup->HasMember( $user->id ), 'user is owner');
    if ( $RT::VERSION =~ /^3/ ) {
        ok( $user->HasRight( Object => $a, Right => 'SeeType' ) && $user->HasRight( Object => $a, Right => 'ShowAsset' ), 'got it' );
    } else {
        $rights = $user->PrincipalObj->HasRights( Object => $a );
        is_deeply( $rights, { SeeType => 1, ShowAsset => 1 }, 'got it' )
    }
}

sub cleanup {
    RTx::AssetTracker::Test->delete_assets( "Type = $qid" );
    RTx::AssetTracker::Test->delete_type_watchers( $type );
};
