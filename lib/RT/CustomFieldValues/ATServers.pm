package RT::CustomFieldValues::ATServers;

# Activate this CFSource via RT_SiteConfig.pm
# Set(@CustomFieldValuesSources, (qw(RT::CustomFieldValues::ATServers)));

use base qw(RT::CustomFieldValues::External);
use RT;
use RTx::AssetTracker::Asset;
use RTx::AssetTracker::Type;

RT::LoadConfig();
RT::Init();
my $at = RTx::AssetTracker::Asset->new(RT->SystemUser);
my $attype = RTx::AssetTracker::Type->new(RT->SystemUser);


sub SourceDescription {
        return 'DC Servers from CMDB';
}


sub ExternalValues {
                my $self = shift;
                my @res;
                my $i = 0;
                my $assets = RTx::AssetTracker::Assets->new($RT::SystemUser);
                my $query = "Status != 'retired' AND Type = 'Servers'";
                my ($id, $msg)  = $assets->FromSQL($query);

                while (my $asset = $assets->Next) {
                        my $assdisc = $asset->Description;
                        my $assname = $asset->Name;
                        push @res,
                                {
                                        name => $assname,
                                        description => $assdisc,
                                        sortorder   => $i++,
                        };

                }

return \@res;
}

RT::Base->_ImportOverlays();

1;
