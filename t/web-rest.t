#!/usr/bin/env perl
use strict;
use warnings;
use RTx::AssetTracker::Test tests => 19;

my ($baseurl, $m) = RTx::AssetTracker::Test->started_ok;

for my $name ("severity", "fu()n:k/") {
    my $cf = RTx::AssetTracker::Test->load_or_create_asset_custom_field(
        Name  => $name,
        Type  => 'Freeform',
        AssetType => 'Servers',
    );
    ok($cf->Id, "created a CustomField");
    is($cf->Name, $name, "correct CF name");
}

my $type = RTx::AssetTracker::Test->load_or_create_type(Name => 'Servers');
ok($type->Id, "loaded the Servers type");

$m->post("$baseurl/REST/1.0/asset/new", [
    user    => 'root',
    pass    => 'password',
    format  => 'l',
]);

my $text = $m->content;
my @lines = $text =~ m{.*}g;
shift @lines; # header

# CFs aren't in the default asset form
push @lines, "CF-fu()n:k/: maximum"; # old style
push @lines, "CF.{severity}: explosive"; # new style

$text = join "\n", @lines;

ok($text =~ s/Name:\s*$/Name: REST interface/m, "successfully replaced name");

$m->post("$baseurl/REST/1.0/asset/edit", [
    user    => 'root',
    pass    => 'password',

    content => $text,
], Content_Type => 'form-data');

my ($id) = $m->content =~ /Asset (\d+) created/;
ok($id, "got asset #$id");

my $asset = RTx::AssetTracker::Asset->new($RT::SystemUser);
$asset->Load($id);
is($asset->Id, $id, "loaded the REST-created asset");
is($asset->Name, "REST interface", "name successfully set");
is($asset->FirstCustomFieldValue("fu()n:k/"), "maximum", "CF successfully set");

$m->post("$baseurl/REST/1.0/search/asset", [
    user    => 'root',
    pass    => 'password',
    query   => "id=$id",
    fields  => "Name,CF-fu()n:k/,CF.{severity},Status",
]);

# the fields are interpreted server-side a hash (why?), so we can't depend
# on order
for ("id: asset/1",
     "Name: REST interface",
     "CF.{fu()n:k/}: maximum",
     "CF.{severity}: explosive",
     "Status: production") {
        $m->content_contains($_);
}

