#!/usr/bin/perl

use Test::More qw(no_plan);

use RT;
use RTx::AssetTracker;
ok(RT::LoadConfig);
ok(RTx::AssetTracker::LoadConfig);
ok(RT::Init, "Basic initialization and DB connectivity");

use File::Find;
File::Find::find({wanted => \&wanted, no_chdir => 1}, 'lib/');
sub wanted { /^*\.pm\z/s && ok(require $_, "Requiring '$_'"); }


