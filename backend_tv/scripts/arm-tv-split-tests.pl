#!/usr/bin/perl

use warnings;
use strict;

use Cwd;
use File::Find;
use File::Basename;
use File::Spec;
use File::Temp;
use Sys::CPU;

## FIXME these tools crash sometimes, should reduce and report triggers

my $ALIVETV = "/home/regehr/alive2-regehr/build/alive-tv";
my $LLVMROOT = "/home/regehr/llvm-project";
my $LLVMBIN = "${LLVMROOT}/for-alive/bin";
my $LLC = "${LLVMBIN}/llc";
my $LLVMAS = "${LLVMBIN}/llvm-as";
my $LLVMNM = "${LLVMBIN}/llvm-nm";
my $LLVMEXTRACT = "${LLVMBIN}/llvm-extract";
my $OPT = "${LLVMBIN}/opt";

my $VERBOSE = 1;

my $NPROCS = Sys::CPU::cpu_count();
print "using $NPROCS cores\n";

# $NPROCS = 1;

my $OWNPATH = File::Basename::dirname(Cwd::abs_path($0));

sub runit ($) {
    (my $cmd) = @_;
    system "$cmd";
    return $? >> 8;
}

my $CWD = getcwd();

my $num = 0;

sub splitFile($) {
    (my $fn) = @_;
    my $count = 0;
    my $bitcode = $fn;
    if ($bitcode =~ s/\.ll$/.bc/) {
        runit("${LLVMAS} $fn");
    }
    open my $NAMES, "${LLVMNM} $bitcode |" or die;
    while (my $line = <$NAMES>) {
        chomp $line;
        if ($line =~ / T (.*)$/) {

	    # print "here3a\n";

	    my $func = $1;
	    $func =~ s/^_//g;
	    print "func = $func\n";
            my $outfn = $CWD . "/" . sprintf("test-%09d.ll", int(rand(1000000000)));
            (my $bcfh, my $bctmpfn) = File::Temp::tempfile();
            (my $bcfh2, my $bctmpfn2) = File::Temp::tempfile();
            runit("${LLVMEXTRACT} --func=${func} $bitcode -S -o ${bctmpfn}");
            runit("grep -v '^; ModuleID =' $bctmpfn | ${OWNPATH}/rename_funcs.pl | grep -v '^target datalayout =' | grep -v '^target triple =' | grep -v '^source_filename =' | ${OPT} -passes=strip - -S -o ${bctmpfn2}");
            unlink($bctmpfn);

            open my $X, "<$bctmpfn2" or die;
            open my $Y, ">$outfn" or die;
            while (my $line = <$X>) {
                chomp $line;
                if ($line =~ /attributes \#([0-9]+) = \{/) {
                    $line = "";
                }
                $line =~ s/ #[0-9]+ / /g;
                print $Y "$line\n";
            }
            close $X;
            close $Y;
            unlink($bctmpfn2);

	    {
		# discard any test that cannot be compiled to arm64 assembly
		(my $fh, my $tmpfn) = File::Temp::tempfile();
		my $res = runit("/usr/bin/timeout --preserve-status 5 ${LLC} -march=aarch64 -o - $outfn > $tmpfn 2>&1");
		if ($res != 0) {
		    print "$fn : can't compile to arm\n" if $VERBOSE;
		    goto discard;
		} else {
		    # goto discard;
		}
	    }

	    {
		# discard any test that Alive2 can't process pretty quickly
		(my $fh, my $tmpfn) = File::Temp::tempfile();
		my $res = runit("/usr/bin/timeout --preserve-status 10 ${ALIVETV} --fail-src-ub=true --always-verify --disable-undef-input --disable-poison-input --smt-to=10000000 $outfn > $tmpfn 2>&1");
		if ($res != 0) {
		    print "$fn : can't process using alive-tv\n" if $VERBOSE;
		    goto discard;
		} else {
		    # goto discard;
		}
	    }

            $count++;
            $num++;

            next;
            
          discard:
            unlink($outfn);
        }
    }
    close $NAMES;
    print "$fn : $count\n" unless $count < 1;
}

my $num_running = 0;
my $opid = $$;

sub wait_for_one() {
    my $xpid = wait();
    die if $xpid == -1;
    $num_running--;
}

sub wanted {
    my $f = $File::Find::name;
    wait_for_one() unless $num_running < $NPROCS;
    die unless $num_running < $NPROCS;
    my $pid = fork();
    die unless $pid >= 0;
    if ($pid == 0) {
	splitFile($f) if ($f =~ /\.ll$/ || $f =~ /\.bc$/);
        exit(0);
    }
    # make sure we're in the parent
    die unless $$ == $opid;
    $num_running++;    
}

my $dir = $ARGV[0];
die "please specify test dir" unless -d $dir;

File::Find::find(\&wanted, $dir);

wait_for_one() while ($num_running > 0);
print "normal termination.\n";
print "remember to run fdupes -dN in the output directory!\n";

