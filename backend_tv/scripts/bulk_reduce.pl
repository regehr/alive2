#!/usr/bin/perl -w

use strict;
use File::Basename;
use Sys::CPU;
use BSD::Resource;

my $NPROCS = Sys::CPU::cpu_count();
print "using $NPROCS cores\n";

my $LLVMREDUCE = $ENV{"HOME"}."/llvm-project/for-alive/bin/llvm-reduce";
my $LLVMDIS = $ENV{"HOME"}."/llvm-project/for-alive/bin/llvm-dis";
my $ARMTV = $ENV{"HOME"}."/alive2-regehr/build/backend-tv";
my $TEST = "test3.sh";

my $INFN = $ARGV[0];
die "please specify a command file" unless -f $INFN;

my $TMPTEST = "_tmp_test.sh";

my $n=0;
open my $INF, "<$INFN" or die;
while (my $line = <$INF>) {
    chomp $line;
    my @stuff = split /\s+/, $line;
    my $func = $stuff[4];
    my $bcfile = $stuff[5];
    open my $TFIN, "<$TEST" or die;
    open my $TFOUT, ">$TMPTEST" or die;
    while (my $l2 = <$TFIN>) {
        $l2 =~ s/FUNC/$func/;
        print $TFOUT $l2;
    }
    close $TFIN;
    close $TFOUT;
    system ("chmod a+rx $TMPTEST");
    my $cmd = "$LLVMREDUCE -test=$TMPTEST $bcfile";
    print "$cmd\n";
    system("rm -f reduced.bc");
    system($cmd);
    system("$LLVMDIS reduced.bc -o reduced_${n}.ll");
    $n++;
}
close $INF;