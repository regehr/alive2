#!/usr/bin/perl -w

use strict;
use File::Basename;
use Sys::CPU;
use BSD::Resource;

# TODO
#
# - the internalize flag is super useful for debugging, but
#   it should be removed once things are working well
#
# - SPEC modules tend to be big, would be very nice to automatically
#   run llvm-reduce on some categories of problems

my $TIMEOUT = 60;

my $GIG = 1000 * 1000 * 1000;
my $MAXKB = 16 * $GIG;
my $ret = setrlimit(RLIMIT_VMEM, $MAXKB, $MAXKB);
die unless $ret;

my $NPROCS = Sys::CPU::cpu_count();
print "using $NPROCS cores\n";

my $LLVMDIS = $ENV{"HOME"}."/llvm-project/for-alive/bin/llvm-dis";
my $ARMTV = $ENV{"HOME"}."/alive2-regehr/build/backend-tv";

my @funcs = ();
my $skipped = 0;

sub scan_file($) {
    (my $file) = @_;
    print ".";
    my $num = 0;
    open my $INF, "$LLVMDIS $file -o - |" or die;
    while (my $line = <$INF>) {
        chomp($line);
        next unless $line =~ /^define /;
        if (!($line =~ /@([a-zA-Z0-9\_\.]+)\(/)) {
            ++$skipped;
            next;
        }
        my $func = $1;
        my @l = ($file, $func, $num++);
        push @funcs, \@l;
    }
    close $INF;
}

sub shuffle ($) {
    my $array = shift;
    my $i;
    for ($i = @$array; --$i; ) {
        my $j = int rand ($i+1);
        next if $i == $j;
        @$array[$i,$j] = @$array[$j,$i];
    }
}

my $num_running = 0;
my $opid = $$;

sub wait_for_one() {
    my $xpid = wait();
    die if $xpid == -1;
    $num_running--;
}

sub go($) {
    (my $cmd) = @_;
    # print "$cmd\n";
    wait_for_one() unless $num_running < $NPROCS;
    die unless $num_running < $NPROCS;
    my $pid = fork();
    die unless $pid >= 0;
    if ($pid == 0) {
        system($cmd);
        exit(0);
    }
    # make sure we're in the parent
    die unless $$ == $opid;
    $num_running++;
}

###########################################

my $dir = $ARGV[0];
die "please specify directory of LLVM bitcode" unless (-d $dir);
my @files = glob "$dir/*.bc";

foreach my $file (@files) {
    scan_file($file);
}

shuffle(\@funcs);

mkdir("logs") or die "oops-- can't create logs directory";

my $count = 0;
my $total = scalar(@funcs);
my $opctstr = "";
print "\n";
foreach my $ref (@funcs) {
    (my $file, my $func, my $num) = @{$ref};
    my ($out, $path, $suffix) = File::Basename::fileparse($file, ".bc");
    my $outfile = "logs/${out}_${num}.log";
    my $cmd = "/usr/bin/timeout -v $TIMEOUT $ARMTV --smt-to=100000000 -internalize -fn $func $file > $outfile 2>&1";
    go($cmd);
    $count++;
    my $pctstr = sprintf("%.1f", $count * 100.0 / $total);
    if ($pctstr ne $opctstr) {
        print("$pctstr %\n");
        $opctstr = $pctstr;
    }
}

wait_for_one() while ($num_running > 0);
print "normal termination.\n";

print "skipped $skipped\n";
print "processed $count\n";

###########################################