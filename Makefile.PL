use inc::Module::Install;
RTx('RTx-AssetTracker');
name('RTx-AssetTracker');
license('GPL version 2');
author('Todd Chapman <todd@chaka.net>');
perl_version('5.008003');
all_from('lib/RTx/AssetTracker.pm');
#requires_rt('3.8.12');
requires(
    #'RT'          => '3.008001',
    'XML::Simple' => 0,
    'XML::Parser' => 0,
    'Set::Scalar' => 0,
);

# XXX: This should be reported by M::I::RTx
my ($lib_path) = $INC{'RT.pm'} =~ /^(.*)[\\\/]/;
my $local_lib_path = "$RT::LocalPath/lib";

substitute( {
        RT_LIB_PATH => join( ' ', $local_lib_path, $lib_path ),
    },
    {
        sufix => '.in',
    },
    't/lib/RTx/AssetTracker/Test.pm',
);

WriteAll();
