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

my $TIMEOUT = $ENV{"TIMEOUT"} || 60;

my $GIG = 1000 * 1000 * 1000;
my $MAXKB = 2 * $GIG;
my $ret = setrlimit(RLIMIT_RSS, $MAXKB, $MAXKB);
die unless $ret;
# RLIMIT_VMEM prevents asan from functioning

my $NPROCS = ($ENV{"NPROCS"} || ( Sys::CPU::cpu_count() - 1)) + 0;
print "using $NPROCS cores\n";

my $LLVMDIS = $ENV{"LLVMDIS"} || $ENV{"HOME"}."/progs/llvm-regehr/llvm/build/bin/llvm-dis";
my $ARMTV = $ENV{"BACKENDTV"} || $ENV{"HOME"}."/progs/alive2-regehr/build/backend-tv";
my $TIMEOUTBIN = $ENV{"TIMEOUTBIN"} || "/usr/bin/timeout";

my @funcs = ();
my $skipped = 0;

sub scan_file($) {
    (my $file) = @_;
#     print ".";
    my $num = 0;

    my $llfile = $file;
    $llfile =~ s/[.]bc$/.ll/;
    my $INF;

    # simply read .ll file if it exists next to the .bc file
    if ($file =~ /[.]ll$/) {
        open $INF, "<$file" or die "failed to read .ll file";
    } elsif (-e $llfile) {
        return; # this is a bc file with a correponsing ll file. assume ll file was found by glob.
    } else {
        open $INF, "$LLVMDIS $file -o - |" or die "failed to llvm-dis";
    }
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

# https://stackoverflow.com/a/19932810
sub difftime2string ($) {
  my ($x) = @_;
  ($x < 0) and return "-" . difftime2string(-$x);
  ($x < 1) and return sprintf("%.2fms",$x*1000);
  ($x < 100) and return sprintf("%.2fsec",$x);
  ($x < 6000) and return sprintf("%.2fmin",$x/60);
  ($x < 108000) and return sprintf("%.2fhrs",$x/3600);
  ($x < 400*24*3600) and return sprintf("%.2fdays",$x/(24*3600));
  return sprintf("%.2f years",$x/(365.25*24*3600));
}

my $num_running = 0;
my $opid = $$;

sub wait_for_one() {
    my $xpid = wait();
    die if $xpid == -1;
    $num_running--;
}

sub go($$) {
    my ($cmd, $outfile) = @_;
    # print "$cmd\n";
    wait_for_one() unless $num_running < $NPROCS;
    die unless $num_running < $NPROCS;
    my $pid = fork();
    die unless $pid >= 0;
    if ($pid == 0) {
        my $start = time();
        system($cmd);
        my $runtime = time() - $start;

        open(my $fh, '>>', $outfile) or die;
        print $fh "\nruntime: $runtime\n";
        close $fh;

        exit(0);
    }
    # make sure we're in the parent
    die unless $$ == $opid;
    $num_running++;
}

###########################################

my $dir = $ARGV[0];
die "please specify directory of LLVM bitcode" unless (-d $dir);
my @files1 = glob "$dir/*.bc";
my @files2 = glob "$dir/*.ll";
my @files = (@files1, @files2);
print "found ", scalar @files, " .bc/.ll files\n";

my $LIMIT = $ENV{"LIMIT"};
if ($LIMIT) {
    splice @files, $LIMIT + 0;
}

while (my ($i, $file) = each @files) {
    print "\r$i" if ($i & ((1 << 8) - 1)) == 0;
    scan_file($file);
}
print "\n";

shuffle(\@funcs);

mkdir("logs") or die "oops-- can't create logs directory";
mkdir("logs-aslp") or die "oops-- can't create logs-aslp directory";

my $starttime = time(); # seconds
my $count = 0;
my $total = scalar(@funcs);
my $opctstr = "";
print "\n";
foreach my $ref (@funcs) {
    (my $file, my $func, my $num) = @{$ref};
    my ($out, $path, $suffix) = File::Basename::fileparse($file, ".bc");
    my $outfile = "logs/${out}_${num}.log";
    my $outfile_aslp = "logs-aslp/${out}_${num}.log";
    my $cmd = "ASLP=false $TIMEOUTBIN -v $TIMEOUT $ARMTV --smt-to=100000000 -internalize -fn $func $file > $outfile 2>&1";
    go($cmd, $outfile);
    $cmd = "ASLP=true $TIMEOUTBIN -v $TIMEOUT $ARMTV --smt-to=100000000 -internalize -fn $func $file > $outfile_aslp 2>&1";
    go($cmd, $outfile_aslp);
    $count++;
    my $pctstr = sprintf("%.2f", $count * 100.0 / $total);
    if ($pctstr ne $opctstr) {
        my $elapsed = time() - $starttime; # seconds
        if ($elapsed <= 0) { $elapsed = 1; }
        my $proportion = ($count / $total);
        my $remaining = ($elapsed / $proportion) * (1.0 - $proportion); # seconds
        my $remainingstr = difftime2string($remaining);
        my $elapsedstr = difftime2string($elapsed);
        my $projectedstr = difftime2string($elapsed + $remaining);
        print("\r$pctstr % ($elapsedstr elapsed / $projectedstr projected, $remainingstr remaining)                ");
        $opctstr = $pctstr;
    }
}
print "\n";

wait_for_one() while ($num_running > 0);
print "normal termination.\n";

print "skipped $skipped\n";
print "processed $count\n";

###########################################
