#!/usr/bin/perl

use warnings;
use strict;
use File::Temp;
use Sys::CPU;
use BSD::Resource;
use File::Basename;

my $CPU_LIMIT = 1 * 60;
my $VMEM_LIMIT = 10 * 1000 * 1000 * 1000;

my $NPROCS = Sys::CPU::cpu_count();
print "using $NPROCS cores\n";

my $LOGDIR = "logs";
mkdir($LOGDIR);

my $ARMTV = "/home/regehr/alive2-regehr/build/backend-tv";

my $ISEL = "";
# my $ISEL = "-global-isel -global-isel-abort=0";

my $XTRA = "";

sub runit ($) {
    (my $cmd) = @_;
    system "$cmd";
    return $? >> 8;
}

my $count = 0;

sub check($) {
    (my $fn) = @_;
    (my $fh, my $tmpfn) = File::Temp::tempfile();
    runit("${ARMTV} $ISEL $XTRA --smt-to=10000000 $fn > $tmpfn 2>&1");
    open my $INF, "<$tmpfn" or die;
    my $data = "";
    while (my $line = <$INF>) {
        $data .= $line;
    }
    close $INF;
    unlink($tmpfn);
    return $data;
}

sub test($) {
    (my $fn) = @_;
    my $basefn = File::Basename::basename($fn, "");
    my $res = check($fn);
    open my $OUTF, ">${LOGDIR}/${basefn}.log" or die;
    print $OUTF "$res\n";
    close $OUTF;
}

my $num_running = 0;
my $opid = $$;

sub wait_for_one() {
    my $xpid = wait();
    die if $xpid == -1;
    $num_running--;
}

sub shuffle {
    my $array = shift;
    my $i;
    for ($i = @$array; --$i; ) {
        my $j = int rand ($i+1);
        next if $i == $j;
        @$array[$i,$j] = @$array[$j,$i];
    }
}

my $PATH = $ARGV[0];
die "please specify location of test cases" unless defined $PATH && -d $PATH;

my @files = ();
push @files, glob "${PATH}/*.bc";
push @files, glob "${PATH}/*.ll";
shuffle(\@files);

my $n = 0;
my $total = scalar(@files);
my $old_pct = -1;

foreach my $fn (@files) {
    $n++;
    my $pct = ($n * 100.0) / $total;
    my $pct_str = sprintf("%.1f", $pct);
    if ($pct_str != $old_pct) {
        print "${pct_str}\%\n";
        $old_pct = $pct_str;
    }
    wait_for_one() unless $num_running < $NPROCS;
    die unless $num_running < $NPROCS;
    my $pid = fork();
    die unless $pid >= 0;
    if ($pid == 0) {
        die "setrlimit CPU" unless setrlimit(RLIMIT_CPU, $CPU_LIMIT, $CPU_LIMIT);
        die "setrlimit VMEM" unless setrlimit(RLIMIT_VMEM, $VMEM_LIMIT, $VMEM_LIMIT);
        test($fn);
        exit(0);
    }
    # make sure we're in the parent
    die unless $$ == $opid;
    $num_running++;
}

wait_for_one() while ($num_running > 0);
print "normal termination.\n";
