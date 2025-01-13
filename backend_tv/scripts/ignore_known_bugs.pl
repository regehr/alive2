#!/usr/bin/perl -w

use strict;
use File::Remove;

my $TIMEOUT = 60;
my $ARMTV = "/home/regehr/alive2-regehr/build/backend-tv --smt-to=10000000";

my @files = glob "*.ll";

my $delete;

my $seen_src_snan;
my $seen_tgt_snan;
my $seen_src_qnan;
my $seen_tgt_qnan;
sub check2($) {
    (my $line) = @_;
    if ($line =~ /Source value: .*SNaN/) {
	$seen_src_snan = 1;
    }
    if ($line =~ /Target value: .*SNaN/) {
	$seen_tgt_snan = 1;
    }
    if ($line =~ /Source value: .*QNaN/) {
	$seen_src_qnan = 1;
    }
    if ($line =~ /Target value: .*QNaN/) {
	$seen_tgt_qnan = 1;
    }
    if ($seen_src_snan && $seen_tgt_snan) {
	$delete = 1;
    }
    if ($seen_src_qnan && $seen_tgt_qnan) {
	$delete = 1;
    }
}

my $seen_pos_zero;
my $seen_neg_zero;
sub check1($) {
    (my $line) = @_;
    if (
	(index($line, "Source value: #x80000000 (-0.0)") != -1) ||
	(index($line, "Target value: #x80000000 (-0.0)") != -1) ||
	(index($line, "Source value: #x8000000000000000 (-0.0)") != -1) ||
	(index($line, "Target value: #x8000000000000000 (-0.0)") != -1)
	) {
	$seen_neg_zero = 1;
    }
    if (
	(index($line, "Source value: #x00000000 (+0.0)") != -1) ||
	(index($line, "Target value: #x00000000 (+0.0)") != -1) ||
	(index($line, "Source value: #x0000000000000000 (+0.0)") != -1) ||
	(index($line, "Target value: #x0000000000000000 (+0.0)") != -1)
	) {
	$seen_pos_zero = 1;
    }
    if ($seen_neg_zero && $seen_pos_zero) {
	$delete = 1;
    }
}

foreach my $f (@files) {
    $seen_pos_zero = 0;
    $seen_neg_zero = 0;
    $seen_src_snan = 0;
    $seen_tgt_snan = 0;
    $seen_src_qnan = 0;
    $seen_tgt_qnan = 0;
    
    $delete = 0;
    print "$f:\n";
    open my $INF, "timeout $TIMEOUT $ARMTV $f |" or die;
    while (my $line = <$INF>) {
	check1($line);
	check2($line);
    }
    close $INF;
    if ($delete) {
	print "  deleting\n";
	File::Remove::remove($f);
    }
}
