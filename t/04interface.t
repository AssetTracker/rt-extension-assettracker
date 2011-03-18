
use strict;
use warnings;

use RTx::AssetTracker::Test;

use RT::CustomField;
use RT::EmailParser;
use RT::Queue;
use RT::Ticket;
use_ok 'RTx::AssetTracker::Type';
use_ok 'RTx::AssetTracker::Asset';

my ($url, $m) = RT::Test->started_ok;

# Variables to test return values
my ($ret, $msg);

# Create a test type
my $type = RTx::AssetTracker::Type->new($RT::SystemUser);
($ret, $msg) = $type->Create('Name' => 'tlaTestType-'.$$,
                              'Description' => 'A general-purpose test type');
ok($ret, "Test type created");


# Create some asset custom fields

my $firstCF = RT::CustomField->new($RT::SystemUser);
my $secondCF = RT::CustomField->new($RT::SystemUser);
($ret, $msg) = $firstCF->Create('Name' => 'First-'.$$,
                         'Type' => 'Text',
                         'MaxValues' => 1,
                         'LookupType' => 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset',
                         'Description' => 'A custom field',
                         'Disabled' => 0);
ok($ret, "First CF created: $msg");
($ret, $msg) = $secondCF->Create('Name' => 'Second-'.$$,
                          'Type' => 'Text',
                          'MaxValues' => 1,
                          'LookupType' => 'RTx::AssetTracker::Type-RTx::AssetTracker::Asset',
                          'Description' => 'Another custom field',
                          'Disabled' => 0);
ok($ret, "Second CF created: $msg");

# Attach the custom fields to our type
($ret, $msg) = $firstCF->AddToObject($type);
ok($ret, "First CF added to type: $msg");
($ret, $msg) = $secondCF->AddToObject($type);
ok($ret, "Second CF added to type: $msg");
my ($aid, $bid) = ($firstCF->Id, $secondCF->Id);

my %cvals = ('asset1a' => 'Some first about swallows',
             'asset1b' => 'Some second about Europe and Africb',
             'asset2a' => 'Another first about Monty Python',
             'asset2b' => 'Romani ite domum',
             'asset3a' => 'Why should I eat my supper?',
             'asset3b' => 'There are starving children in Africb',
             'asset4a' => 'What did Brian originally write?',
             'asset4b' => 'Romanes eunt domus');

# Create an asset or two with our custom field values.

my $asset1 = RTx::AssetTracker::Asset->new($RT::SystemUser);
my $asset2 = RTx::AssetTracker::Asset->new($RT::SystemUser);
my $asset3 = RTx::AssetTracker::Asset->new($RT::SystemUser);
my $asset4 = RTx::AssetTracker::Asset->new($RT::SystemUser);
($ret, $msg) = $asset1->Create(Name => 'First asset '.$$,
                                 Description => 'blah blah 1',
                                 Type => $type->Id,
                                 "CustomField-$aid" => $cvals{'asset1a'},
                                 "CustomField-$bid" => $cvals{'asset1b'},
                                 );
ok($ret, "asset 1 created");
($ret, $msg) = $asset2->Create(Name => 'Second asset '.$$,
                                 Description => 'foo bar 2',
                                 Type => $type->Id,
                                 "CustomField-$aid" => $cvals{'asset2a'},
                                 "CustomField-$bid" => $cvals{'asset2b'},
                                 );
ok($ret, "asset 2 created");
($ret, $msg) = $asset3->Create(Name => 'Third asset '.$$,
                                 Description => 'ping pong 3',
                                 Type => $type->Id,
                                 "CustomField-$aid" => $cvals{'asset3a'},
                                 "CustomField-$bid" => $cvals{'asset3b'},
                                 );
ok($ret, "asset 3 created");
($ret, $msg) = $asset4->Create(Name => 'Fourth asset '.$$,
                                 Description => 'hoi polloi 4',
                                 Type => $type->Id,
                                 "CustomField-$aid" => $cvals{'asset4a'},
                                 "CustomField-$bid" => $cvals{'asset4b'},
                                 );
ok($ret, "asset 4 created");

# Create a ticket.
my $parser = RT::EmailParser->new();
$parser->ParseMIMEEntityFromScalar('From: root@localhost
To: rt@example.com
Subject: test ticket for assets

This is some form of new request.
There is a problem with an asset.');

my $ticket = RT::Ticket->new($RT::SystemUser);
my $obj;
($ret, $obj, $msg) = $ticket->Create(Queue => 'General',
                               Subject => 'test ticket for assets '.$$,
                               MIMEObj => $parser->Entity);
ok($ret, "Test ticket for assets created: $msg");


#### Right.  That's our data.  Now begin the real testing.

isa_ok($m, 'Test::WWW::Mechanize');
ok($m->login, 'logged in');
$m->follow_link_ok({text => 'Assets'}, 'UI -> AssetTracker');
$m->content_contains($asset3->Name);
$m->follow_link_ok( {text => $asset3->Name}, 'AssetTracker -> '. $asset3->Name );
$m->title_is("Asset #" . $asset3->Id . ": " . $asset3->Name);
$m->follow_link_ok( {text => 'Links'}, 'Asset -> ModifyLinks' );


{
my $a1uri = 'at://example.com/asset/'.$asset1->Id;
my $a3uri = 'at://example.com/asset/'.$asset3->Id;

$m->content_like(qr/Find assets/, "found asset search box");
$m->submit_form(with_fields => { 'AssetField'  => $aid,
                                 'AssetOp'     => '=',
                                 'AssetString' => $cvals{'asset1a'} } );
$m->content_contains($asset1->Name, "found " . $asset1->Name);
$m->submit_form(with_fields => { "AddLink-Asset-$a1uri" => 'ComponentOf' } );

$m->content_contains("URI $a3uri component of URI $a1uri", "Asset component link was created");
}

# Now try to create a linked ticket.
$m->get_ok($url."/AssetTracker/Asset/Display.html?id=".$asset2->Id, 
           "Loaded asset display");
$m->content_like(qr/New ticket in queue/, "Ticket creation link shows up");
$m->submit_form(form_name => 'child', fields => { 'Queue' => '1' } );
$m->submit_form(form_name => 'TicketCreate', fields => { 'Content' => 'This asset has a problem that needs fixing' } );

{
my $a2name = $asset2->Name;
my $a2id = $asset2->Id;
$m->title_like(qr/Problem with $a2name/, 'Asset -> Linked ticket');
$m->content_contains("asset #$a2id: $a2name",
                     "Ticket references originating asset");
}
