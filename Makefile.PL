use inc::Module::Install;
RTx('RTx-AssetTracker');
name('RTx-AssetTracker');
license('GPL version 2');
perl_version('5.008003');
version '1.002005';
all_from('lib/RTx/AssetTracker.pm');
requires(
    'RT'                => '3.8.1',
);

WriteAll();
