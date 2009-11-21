use strict;
use warnings;

use Test::Most;
use lib '/home/gabor/work/Test-Snapshots/lib';
use Test::Snapshots;

bail_on_fail();

Test::Snapshots::command('bin/cobc_wrapper');
Test::Snapshots::debug(1);
#Test::Snapshots::combine(1);
Test::Snapshots::set_glob('*.cob');

#Test::Snapshots::skip({
#	'examples/add.cob'   => 'reason',
#});

Test::Snapshots::test_all_snapshots('examples');



__END__
use Test::More;
#use Text::Diff qw(diff);
use Getopt::Long qw(GetOptions);
my $tests = 0;

# on the command line one can give names of .in files
my @in = <examples/*.in>;
my %opt = (
    mode => '',
    std  => '',
);

GetOptions(\%opt,
    "mode=s",
    "std=s",
) or die;
my $opt = '';
foreach my $f (qw(mode std)) {
    if ($opt{$f}) {
        $opt .= " --$f $opt{$f}";
    }
}

if (@ARGV) {
    @in = @ARGV;
}

plan tests => $tests + @in*2;
my $cobol = './bin/acme-cobol';

foreach my $in (@in) {
    my $file = substr $in, 0, -3;
    (my $name = $file) =~ s/\.\d+$//;
    my $cmd = "$cobol $opt $name.cob >out 2>err <$in";
    diag $cmd;
    system $cmd;
    foreach my $std (qw(out err)) {
        my $diff = diff "$file.$std", $std; 
        is($diff, '', "$in $std") or diag "cp $std $file.$std";
    }
#    BEGIN { $tests += 1; }
}


