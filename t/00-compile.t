
use strict;
use warnings;

use lib 't/lib';
use RTx::AssetTracker::Test tests => 42;

require_ok("RTx::AssetTracker");
require_ok("RTx::AssetTracker::Test");
require_ok("RTx::AssetTracker::Asset");
require_ok("RTx::AssetTracker::Assets");
require_ok("RTx::AssetTracker::IP");
require_ok("RTx::AssetTracker::IPs");
require_ok("RTx::AssetTracker::Port");
require_ok("RTx::AssetTracker::Ports");
require_ok("RTx::AssetTracker::Record");
require_ok("RTx::AssetTracker::Scrip");
require_ok("RTx::AssetTracker::Scrips");
require_ok("RTx::AssetTracker::ScripAction");
require_ok("RTx::AssetTracker::ScripActions");
require_ok("RTx::AssetTracker::ScripCondition");
require_ok("RTx::AssetTracker::ScripConditions");
require_ok("RTx::AssetTracker::SearchBuilder");
require_ok("RTx::AssetTracker::System");
require_ok("RTx::AssetTracker::Template");
require_ok("RTx::AssetTracker::Templates");
require_ok("RTx::AssetTracker::Type");
require_ok("RTx::AssetTracker::Types");

require_ok("RT::Graph::AssetTracker::Assets");
require_ok("RT::Shredder::Plugin::AssetObjects");
require_ok("RT::Shredder::Plugin::Assets");
require_ok("RT::URI::at");

require_ok("RT::CustomField");
require_ok("RT/CustomField_Vendor.pm");
require_ok("RT::CustomFields");
require_ok("RT/CustomFields_Vendor.pm");
require_ok("RT::Interface::Web");
require_ok("RT/Interface/Web_Vendor.pm");
require_ok("RT::Interface::Web::QueryBuilder::Tree");
require_ok("RT/Interface/Web/QueryBuilder/Tree_Vendor.pm");
require_ok("RT::System");
require_ok("RT/System_Vendor.pm");
require_ok("RT::Ticket");
require_ok("RT/Ticket_Vendor.pm");
require_ok("RT::Transaction");
require_ok("RT/Transaction_Vendor.pm");


# no the following doesn't work yet
__END__
use File::Find::Rule;

my @files = File::Find::Rule->file()
    ->name( '*.pm' )
    ->in( 'lib' );

plan tests => scalar @files;

for (@files) {
    local $SIG{__WARN__} = sub {};
    require_ok($_);
}

