
use strict;
use warnings;

use RTx::AssetTracker::Test tests => 65;
use RT;


{

use_ok ('RTx::AssetTracker::Type');
ok(my $testtype = RTx::AssetTracker::Type->new($RT::SystemUser));
ok($testtype->Create( Name => 'asset tests'));
isnt($testtype->Id , 0);
use_ok('RT::CustomField');
ok(my $testcf = RT::CustomField->new($RT::SystemUser));
my ($ret, $cmsg) = $testcf->Create( Name => 'selectmulti',
                                    LookupType => 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset',
                                    Type => 'SelectMultiple');
ok($ret,"Created the custom field - ".$cmsg);

($ret, $cmsg) = $testcf->AddToObject($testtype);
ok($ret, "CF added to type - $cmsg");

($ret,$cmsg) = $testcf->AddValue ( Name => 'Value1',
                        SortOrder => '1',
                        Description => 'A testing value');

ok($ret, "Added a value - ".$cmsg);

ok($testcf->AddValue ( Name => 'Value2',
                        SortOrder => '2',
                        Description => 'Another testing value'));
ok($testcf->AddValue ( Name => 'Value3',
                        SortOrder => '3',
                        Description => 'Yet Another testing value'));
                       
is($testcf->Values->Count , 3);

use_ok('RTx::AssetTracker::Asset');

my $u = RT::User->new($RT::SystemUser);
$u->Load("root");
ok ($u->Id, "Found the root user");
ok(my $a = RTx::AssetTracker::Asset->new($RT::SystemUser));
ok(my ($id, $msg) = $a->Create( Type => $testtype->Id,
               Name => 'Testing',
               Owner => $u->Id
              ));
isnt($id , 0);
is ($a->OwnerRoleGroup->UserMembersObj->First->Id , $u->Id, "Root is the asset owner");
ok(my ($cfv, $cfm) =$a->AddCustomFieldValue(Field => $testcf->Id,
                           Value => 'Value1'));
isnt($cfv , 0, "Custom field creation didn't return an error: $cfm");
is($a->CustomFieldValues($testcf->Id)->Count , 1);
ok($a->CustomFieldValues($testcf->Id)->First &&
    $a->CustomFieldValues($testcf->Id)->First->Content eq 'Value1');

ok(my ($cfdv, $cfdm) = $a->DeleteCustomFieldValue(Field => $testcf->Id,
                        Value => 'Value1'));
isnt ($cfdv , 0, "Deleted a custom field value: $cfdm");
is($a->CustomFieldValues($testcf->Id)->Count , 0);

ok(my $a2 = RTx::AssetTracker::Asset->new($RT::SystemUser));
ok($a2->Load($id));
is($a2->Name, 'Testing');
is($a2->TypeObj->Id, $testtype->id);
is($a2->OwnerRoleGroup->UserMembersObj->First->Id, $u->Id);

my $a3 = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($id3, $msg3) = $a3->Create( Type => $testtype->Id,
                                Name => 'Testing 2',
                                Owner => $u->Id);
my ($cfv1, $cfm1) = $a->AddCustomFieldValue(Field => $testcf->Id,
 Value => 'Value1');
isnt($cfv1 , 0, "Adding a custom field to asset 1 is successful: $cfm1");
my ($cfv2, $cfm2) = $a3->AddCustomFieldValue(Field => $testcf->Id,
 Value => 'Value2');
isnt($cfv2 , 0, "Adding a custom field to asset 2 is successful: $cfm2");
my ($cfv3, $cfm3) = $a->AddCustomFieldValue(Field => $testcf->Id,
 Value => 'Value3');
isnt($cfv3 , 0, "Adding a custom field to asset 1 is successful: $cfm3");
is($a->CustomFieldValues($testcf->Id)->Count , 2,
   "This asset has 2 custom field values");
is($a3->CustomFieldValues($testcf->Id)->Count , 1,
   "This asset has 1 custom field value");


}

{


ok(require RTx::AssetTracker::Asset, "Loading the RTx::AssetTracker::Asset library");


}

{

my $a = RTx::AssetTracker::Asset->new($RT::SystemUser);

ok( $a->Create(Type => 'Servers', ReferredToBy => 'http://www.cpan.org', RefersTo => 'http://fsck.com', Name => 'This is a name'), "Asset Created");

ok ( my $id = $a->Id, "Got asset id");
like ($a->RefersTo->First->Target , qr/fsck.com/, "Got refers to");
like ($a->ReferredToBy->First->Base , qr/cpan.org/, "Got referredtoby");


}

{

ok(my $jesse = RT::User->new($RT::SystemUser), "Creating a jesse rt::user");
$jesse->LoadOrCreateByEmail('jesse@example.com');
ok($jesse->Id,  "Found the jesse rt user");

my $asset = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($id, $msg) = $asset->Create(Name => "Foo",
                Owner => $RT::SystemUser->Id,
                Status => 'production',
                Admin => ['jesse@example.com'],
                Type => '1'
                );
ok ($id, "Asset $id was created");
ok(my $group = $asset->LoadAssetRoleGroup(Type=> 'Admin'));
ok ($group->Id, "Found the admins object for this asset");


ok ($asset->IsWatcher(Type => 'Admin', PrincipalId => $jesse->PrincipalId), "The asset actually has jesse at fsck.com as a admin");
ok(my $bob = RT::User->new($RT::SystemUser), "Creating a bob rt::user");
$bob->LoadOrCreateByEmail('bob@fsck.com');
ok($bob->Id,  "Found the bob rt user");
ok (my ($add_id, $add_msg) = $asset->AddWatcher(Type => 'Admin', Email => 'bob@fsck.com'), "Added bob at fsck.com as a admin");
ok ($add_id, "Add succeeded: ($add_msg)");
ok ($asset->IsWatcher(Type => 'Admin', PrincipalId => $bob->PrincipalId), "The asset actually has bob at fsck.com as a admin");
ok ( ($add_id, $add_msg) = $asset->DeleteWatcher(Type =>'Admin', Email => 'bob@fsck.com'), "Added bob at fsck.com as a admin");
ok (!$asset->IsWatcher(Type => 'Admin', PrincipalId => $bob->PrincipalId), "The asset no longer has bob at fsck.com as a admin");

$group = $asset->LoadAssetRoleGroup(Asset => $id, Type=> 'Owner');
ok ($group->Id, "Found the Owner object for this asset");
ok($group->HasMember($RT::SystemUser->UserObj->PrincipalObj), "the owner group has the member 'RT_System'");


}

{

my $aa = RTx::AssetTracker::Asset->new($RT::SystemUser);
my ($id, $tid, $msg)= $aa->Create(Type => 'servers',
            Name => 'test');
ok($id, $msg);
is($aa->Status, 'production', "New asset is created as production");

($id, $msg) = $aa->SetStatus('test');
ok($id, $msg);
like($msg, qr/test/i, "Status message is correct");
($id, $msg) = $aa->SetStatus('retired');
ok($id, $msg);
like($msg, qr/retired/i, "Status message is correct");
($id, $msg) = $aa->SetStatus('retired');
ok(!$id,$msg);



}

1;
