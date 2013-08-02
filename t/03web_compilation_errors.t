#!/usr/bin/perl

use strict;
use warnings;
use RTx::AssetTracker::Test no_plan => 1;

use RT;
RT::LoadConfig;
RT::Init;

our @a = RTx::AssetTracker::Test->create_assets(
    { Type => 1 },
    { },
);

my ($url, $agent) = RT::Test->started_ok;

$agent->get($url);

is ($agent->{'status'}, 200, "Loaded a page");


# {{{ test a login
ok($agent->login, 'logged in');

is($agent->{'status'}, 200, "Fetched the page ok");
ok( $agent->{'content'} =~ /Logout/i, "Found a logout link");

use File::Find;
find ( \&wanted , 'html/AssetTracker/');

sub wanted {
    -f  && /\.html$/ && $_ !~ /Logout.html$/  && test_get($File::Find::name);
}       

sub test_get {
    my $file = shift;

    $file =~ s#^html/##;

    if ( $file =~ m#^AssetTracker/Admin/Types/Scrip\.html$# ) {
        $file .= '?AssetType=1';
    } elsif ( $file =~ m#^AssetTracker/Admin/Global/Template\.html$# ) {
        $file .= '?Create=1';
    } elsif ( $file =~ m#^AssetTracker/Admin/Types/Template\.html$# ) {
        $file .= '?Create=1&AssetType=1';
    } elsif ( $file =~ m#^AssetTracker/Admin/Types/(.*)\.html$# ) {
        $file .= '?id=1';
    } elsif ( $file =~ m#^AssetTracker/Asset/Create\.html$# ) {
        $file .= '?Type=1';
    } elsif ( $file =~ m#^AssetTracker/Asset/(.*)\.html$# ) {
        $file .= '?id=' . $a[0]->Id;
    } elsif ( $file =~ m#^AssetTracker/Search/(.*)\.html$# ) {
        $file .= '?Query=id>0&Rows=10&Format=id';
    }

    ok ($agent->get("$url/$file"), "GET $url/$file");
    is ($agent->{'status'}, 200, "Loaded $file");
#    ok( $agent->{'content'} =~ /Logout/i, "Found a logout link on $file ");
    ok( $agent->{'content'} !~ /Not logged in/i, "Still logged in for  $file");
    ok( $agent->{'content'} !~ /System error/i, "Didn't get a Mason compilation error on $file");
        
}

# }}}

1;
