use strict;
use warnings FATAL => 'all';

use Test::More;
use Text::Diff qw(diff);
use Getopt::Long qw(GetOptions);
my $tests = 0;

# on the command line one can give names of .in files
my @in = <examples/*.in>;
my $mode = '';
GetOptions("mode=s" => \$mode) or die;
if ($mode) {
    $mode = "--mode $mode";
}
if (@ARGV) {
    @in = @ARGV;
}

plan tests => $tests + @in*2;
my $cobol = './bin/acme-cobol';

foreach my $in (@in) {
    my $file = substr $in, 0, -3;
    (my $name = $file) =~ s/\.\d+$//;
    my $cmd = "$cobol $mode $name.cob >out 2>err <$in";
    diag $cmd;
    system $cmd;
    foreach my $std (qw(out err)) {
        my $diff = diff "$file.$std", $std; 
        is($diff, '', "$in $std") or diag "cp $std $file.$std";
    }
#    BEGIN { $tests += 1; }
}


