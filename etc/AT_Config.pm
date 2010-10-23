#
# WARNING: NEVER EDIT AT_Config.pm. Instead, copy any sections you want to change to AT_SiteConfig.pm
# and edit them there.
#

=head1 NAME

AT::Config

=for testing

use AT::Config;

=cut

# $DefaultAssetSearchResultFormat is the default format for RT search results
Set ($DefaultAssetSearchResultFormat, 
 qq{'<B><A HREF="$RT::WebPath/AssetTracker/Asset/Display.html?id=__id__">__Name__</a></B>/TITLE:Name',
   Description,
   Status,
   TypeName, 
   }
   );


# {{{ Miscellaneous AT Settings

# You can define new statuses and even reorder existing statuses here.
# WARNING. DO NOT DELETE ANY OF THE DEFAULT STATUSES. If you do, RT
# will break horribly.

@AssetActiveStatus = qw(production development qa dr pilot test) unless @AssetActiveStatus;
@AssetInactiveStatus = qw(retired) unless @AssetInactiveStatus;

# }}}

# Asset name uniqueness is now enforced by the API instead of the DB
# The rules are evaluated in this order:
Set ($GlobalUniqueAssetName, 1);
Set ($TypeUniqueAssetName, 0);
Set ($TypeStatusUniqueAssetName, 0);

# If you don't want to use IPs, or would rather use CFs for IPs
# then set this to zero to disable IP features in AT
Set ($EnableIP, 1);

# Control if a user needs ModifyAsset on both assets to create
# a link between them
Set ($ModifyBothAssetsForLink, 0);

# The number of history items to display on the Asset main page
# (set to 0 to turn  off)
Set ($ShowAssetHistory, 10);

# You can define new Link types by defining a list with an even
# number of elements. The first element in each pair is the
# forward link and the second element is the reverse link.

# @AssetLinkTypes = qw( LocatedAt AtThisLocation );

# List of role names. Need more/less/different roles? Change it!
# @AssetRoles = qw( Admin Owner ); #default

# When displaying asset watchers descend into groups and show
# the user members. Turning this off make the display more succinct.
Set ($ShowGroupMembers, 1);

# If you turn this on not only will the asset display
# show watchers directly assigned to the asset, but type 
# watchers will also display. This may clear up confusion 
# where users think there is no watcher assigned when there really is.
Set ($ShowTypeWatchersInAsset, 0);

1;
