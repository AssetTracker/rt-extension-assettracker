use inc::Module::Install;
RTx('RTx-AssetTracker');
name('RTx-AssetTracker');
license('GPL version 2');
author('Todd Chapman <todd@chaka.net>');
perl_version('5.008003');
all_from('lib/RTx/AssetTracker.pm');
requires(
    #'RT'          => '3.008001',
    'XML::Simple' => 0,
    'XML::Parser' => 0,
    'Set::Scalar' => 0,
);

WriteAll();
