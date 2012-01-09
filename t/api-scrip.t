
use strict;
use warnings;

use RTx::AssetTracker::Test tests => 25;

my $assettype = RT::Test->load_or_create_assettype( Name => 'General' );
ok $assettype && $assettype->id, 'loaded or created asset type';

note 'basic scrips functionality test: create+execute';
{
    my $s1 = RTx::AssetTracker::Scrip->new(RT->SystemUser);
    my ($val, $msg) = $s1->Create(
        Assettype => $assettype->Id,
        ScripAction => 'User Defined',
        ScripCondition => 'User Defined',
        CustomIsApplicableCode => '$self->AssetObj->Name =~ /fire/? 1 : 0',
        CustomPrepareCode => 'return 1',
        CustomCommitCode => '$self->AssetObj->SetDescription("87");',
        Template => 'Blank'
    );
    ok($val,$msg);

    my $asset = RTx::AssetTracker::Asset->new(RT->SystemUser);
    my ($tv,$ttv,$tm) = $asset->Create(
        Type => $assettype->Id,
        Name => "hair on fire",
    );
    ok($tv, $tm);

    is ($asset->Description , 'firey', "Asset description is set right");

    my $asset2 = RT::Asset->new(RT->SystemUser);
    my ($t2v,$t2tv,$t2m) = $asset2->Create(
        Type => $assettype->Id,
        Name => "hair in water",
    );
    ok($t2v, $t2m);
    isnt ($asset2->Description , 'firey', "Asset description is set right");
}


{
    my $scrip = RTx::AssetTracker::Scrip->new($RT::SystemUser);
    my ( $val, $msg ) = $scrip->Create(
        ScripCondition => 'User Defined',
        ScripAction    => 'User Defined',
    );
    ok( !$val, "missing template: $msg" );
    ( $val, $msg ) = $scrip->Create(
        ScripCondition => 'User Defined',
        ScripAction    => 'User Defined',
        Template       => 'not exists',
    );
    ok( !$val, "invalid template: $msg" );

    ( $val, $msg ) = $scrip->Create(
        ScripAction => 'User Defined',
        Template    => 'Blank',
    );
    ok( !$val, "missing condition: $msg" );
    ( $val, $msg ) = $scrip->Create(
        ScripCondition => 'not exists',
        ScripAction    => 'User Defined',
        Template       => 'Blank',
    );
    ok( !$val, "invalid condition: $msg" );

    ( $val, $msg ) = $scrip->Create(
        ScripCondition => 'User Defined',
        Template       => 'Blank',
    );
    ok( !$val, "missing action: $msg" );
    ( $val, $msg ) = $scrip->Create(
        ScripCondition => 'User Defined',
        ScripAction    => 'not exists',
        Template       => 'Blank',
    );
    ok( !$val, "invalid action: $msg" );

    ( $val, $msg ) = $scrip->Create(
        ScripAction    => 'User Defined',
        ScripCondition => 'User Defined',
        Template       => 'Blank',
    );
    ok( $val, "created scrip: $msg" );
    $scrip->Load($val);
    ok( $scrip->id, 'loaded scrip ' . $scrip->id );

    ( $val, $msg ) = $scrip->SetScripCondition();
    ok( !$val, "missing condition: $msg" );
    ( $val, $msg ) = $scrip->SetScripCondition('not exists');
    ok( !$val, "invalid condition: $msg" );
#    ( $val, $msg ) = $scrip->SetScripCondition('On Correspond');
#    ok( $val, "updated condition to 'On Correspond': $msg" );

    ( $val, $msg ) = $scrip->SetScripAction();
    ok( !$val, "missing action: $msg" );
    ( $val, $msg ) = $scrip->SetScripAction('not exists');
    ok( !$val, "invalid action: $msg" );
#    ( $val, $msg ) = $scrip->SetScripAction('Notify AdminCcs');
#    ok( $val, "updated action to 'Notify AdminCcs': $msg" );

    ( $val, $msg ) = $scrip->SetTemplate();
    ok( !$val, "missing template $msg" );
    ( $val, $msg ) = $scrip->SetTemplate('not exists');
    ok( !$val, "invalid template $msg" );
#    ( $val, $msg ) = $scrip->SetTemplate('Forward');
#    ok( $val, "updated template to 'Forward': $msg" );

    ok( $scrip->Delete, 'delete the scrip' );
}

1;
