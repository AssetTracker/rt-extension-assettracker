
use strict;
use warnings;

use RTx::AssetTracker::Test tests => 25;
use RT;

{

ok (require RTx::AssetTracker::Scrip);


my $t = RTx::AssetTracker::Type->new($RT::SystemUser);
$t->Create(Name => 'ScripTest');
ok($t->Id, "Created a scriptest type");

my $s1 = RTx::AssetTracker::Scrip->new($RT::SystemUser);
my ($val, $msg) =$s1->Create( AssetType => $t->Id,
             ScripAction => 'User Defined',
             ScripCondition => 'User Defined',
             CustomIsApplicableCode => 'if ($self->AssetObj->Name =~ /fire/) { return (1);} else { return(0)}',
             CustomPrepareCode => 'return 1',
             CustomCommitCode => '$self->AssetObj->SetDescription(Value => "firey");',
             Template => 'Blank'
    );
ok($val,$msg);

my $asset = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($av,$atv,$am) = $asset->Create(Type => $t->Id,
                                    Name => "hair on fire",
                                    );
ok($av, $am);

is ($asset->Description , 'firey', "Asset description is set right");


my $asset2 = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($t2v,$t2tv,$t2m) = $asset2->Create(Type => $t->Id,
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
