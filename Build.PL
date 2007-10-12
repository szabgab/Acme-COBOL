use strict;
use warnings;
use Module::Build;

my $build = Module::Build->new(
    module_name        => 'Acme::COBOL',
    dist_version_from  => 'lib/Acme/COBOL.pm',
    license            => 'perl',
    create_makefile_pl => 'traditional',
    script_files       => 'bin/acme-cobol',
    requires           => {
            'Test::More'           => '0',
            'Text::Diff'           => '0',
            'File::Slurp'          => '0',
    },
);

$build->create_build_script;

