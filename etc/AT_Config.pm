#
# WARNING: NEVER EDIT AT_Config.pm. Instead, copy any sections you want to change to AT_SiteConfig.pm
# and edit them there.
#

=head1 NAME

AT::Config

=for testing

use AT::Config;

=cut

=item C<$AssetsItemMapSize>

On the display page of a asset from search results, RT provides links
to the first, next, previous and last asset from the results.  In
order to build these links, RT needs to fetch the full result set from
the database, which can be resource-intensive.

Set C<$AssetsItemMapSize> to number of assets you want RT to examine
to build these links. If the full result set is larger than this
number, RT will omit the "last" link in the menu.  Set this to zero to
always examine all results.

=cut

Set($AssetsItemMapSize, 1000);

# $DefaultAssetSearchResultFormat is the default format for AT search results
Set ($DefaultAssetSearchResultFormat, 
 qq{'<B><A HREF="$RT::WebPath/AssetTracker/Asset/Display.html?id=__id__">__Name__</a></B>/TITLE:Name',
   Description,
   Status,
   TypeName, 
   }
   );


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

# You can define new Link types by adding new key => value pairs.
# The key in each pair is the forward link and the value element is
# the reverse link.

# Set(%AssetLinkTypes, LocatedAt => 'AtThisLocation' );

Set(%AssetLinkTypes,
    RunsOn      => 'IsRunning',
    RefersTo    => 'ReferredToBy',
    DependsOn   => 'DependedOnBy',
    ComponentOf => 'HasComponent',
);

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

=item C<%AdminSearchResultFormat>

In admin interface format strings similar to tickets search result
formats are used. Use C<%AdminSearchResultFormat> to define format
strings per RT class.

=cut

Set(%AdminSearchResultFormat,
    AT_Types =>
        q{'<a href="__WebPath__/AssetTracker/Admin/Types/Modify.html?id=__id__">__id__</a>/TITLE:#'}
        .q{,'<a href="__WebPath__/AssetTracker/Admin/Types/Modify.html?id=__id__">__Name__</a>/TITLE:Name'}
        .q{,__Description__,__Disabled__},

    AT_Scrips =>
        q{'<a href="__WebPath__/AssetTracker/Admin/Types/Scrip.html?id=__id__&AssetType=__AssetTypeId__">__id__</a>/TITLE:#'}
        .q{,'<a href="__WebPath__/AssetTracker/Admin/Types/Scrip.html?id=__id__&AssetType=__AssetTypeId__">__Description__</a>/TITLE:Description'
}
        .q{,__Stage__, __Condition__, __Action__, __Template__},

    AT_GlobalScrips =>
        q{'<a href="__WebPath__/AssetTracker/Admin/Global/Scrip.html?id=__id__">__id__</a>/TITLE:#'}
        .q{,'<a href="__WebPath__/AssetTracker/Admin/Global/Scrip.html?id=__id__">__Description__</a>/TITLE:Description'
}
        .q{,__Stage__, __Condition__, __Action__, __Template__},

    AT_Templates =>
        q{'<a href="__WebPath__/__WebRequestPathDir__/Template.html?AssetType=__AssetTypeId__&Template=__id__">__id__</a>/TITLE:#'}
        .q{,'<a href="__WebPath__/__WebRequestPathDir__/Template.html?AssetType=__AssetTypeId__&Template=__id__">__Name__</a>/TITLE:Name'}
        .q{,'__Description__'},
);


local $rt_comps = RT->Config->Get("HomepageComponents");
RT->Config->Set("HomepageComponents", [@$rt_comps, qw(AssetQuickSearch)]);


=head1 Lifecycles

=cut

Set(%Lifecycles,
    at_default => {
        initial         => [ ],
        active          => [ 'production', 'development', 'qa', 'dr', 'pilot', 'test' ],
        inactive        => [ 'retired', 'deleted' ],

        defaults => {
            on_create => 'production',
        },

        transitions => {
            ''       => [qw(production development qa dr pilot test retired)],

            # from   => [ to list ],
            production  => [qw(development qa dr pilot test retired deleted)],
            development => [qw(production qa dr pilot test retired deleted)],
            qa          => [qw(production development dr pilot test retired deleted)],
            dr          => [qw(production development qa pilot test retired deleted)],
            pilot       => [qw(production development qa dr test retired deleted)],
            test        => [qw(production development qa dr pilot retired deleted)],
            retired     => [qw(production development qa dr pilot test deleted)],
            deleted     => [qw(production development qa dr pilot test retired)],
        },
        rights => {
            '* -> retired'  => 'RetireAsset',
            '* -> deleted'  => 'DeleteAsset',
            '* -> *'        => 'ModifyAsset',
        },
        actions => [
            'production -> retired' => {
                label => 'Retire',
            },
            '* -> deleted' => {
                label => 'Delete',
            }
        ],
    },
);


1;
