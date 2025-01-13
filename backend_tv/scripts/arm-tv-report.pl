#!/usr/bin/perl

use warnings;
use strict;
use File::Copy;

my %results = ();
my %triggers = ();
my %logs = ();
my %missing_vec = ();

my $PATH = $ARGV[0];
die "please specify location of test cases" unless defined $PATH && -d $PATH;

mkdir("results");

my $CORRECT = "Translation validated as correct";

sub classify($) {
    (my $data) = @_;

    if ($data =~ /^(ERROR: Only the C and fast calling conventions are supported)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: inline assembly not supported)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: we don't support structures in return values)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: Unsupported function argument.*)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: varargs not supported.*)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: only float and double supported.*)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: Only vectors of integer, f32, f64.*)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: Couldn't prove the correctness.*)/m) {
        return "[u] $1";
    }
    if ($data =~ /^(ERROR: Only short vectors 8 and 16 bytes long.*)/m) {
        return "[u] $1";
    }   
    if ($data =~ /^(LLVM ERROR: Cannot select.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Unsupported AArch64 instruction: BRK)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Unsupported metadata.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Source function is always UB.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Too many temporaries.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: NZCV is the only supported case for MSR.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: NZCV is the only supported case for MRS.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: SMT Error.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Unsupported instruction.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: The source program doesn't reach a return instruction.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Unsupported type.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Unsupported attribute.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Could not translate.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /^(ERROR: Not supporting.*)/m) {
	return "[u] $1";
    }
    if ($data =~ /ERROR: UB triggered/m) {
	return "[f] UB triggered";
    }
    if ($data =~ /ERROR: Value mismatch/m) {
        return "[f] Value mismatch";
    }
    if ($data =~ /^(error: .*)$/m) {
        return "[f] $1";
    }
    if ($data =~ /^(ERROR: .*)$/m) {
        my $ret = $1;
        if ($data =~ /Unsupported AArch64 instruction: ([\_a-zA-Z0-9]+)/) {
            my $insn = $1;
            if ($insn =~ /^([a-zA-Z0-9]+)v[0-9]+i[0-9]+/) {
                my $root = $1;
                $missing_vec{$root}++;
            }
            return "[i] $ret";
        }
        return "[f] $ret";
    }
    if ($data =~ /(Aslp assertion failure.*)$/m) {
        return "[f] $1";
    }
    if ($data =~ /(.*Assertion .*)$/m) {
        return "[f] $1";
    }
    if ($data =~ /(UNREACHABLE.*)$/m) {
        return "[f] $1";
    }
    if ($data =~ /Killed$/m) {
        return "[t] Timed out";
    }
    if ($data =~ /seems to be correct/m) {
        return "[c] $CORRECT";
    }
    if ($data =~ /^(LLVM ERROR: .*)$/m) {
        return "[f] $1";
    }
    if ($data =~ /(Segmentation fault)/m) {
        return "[f] $1";
    }
    return "[f] Unknown failure";
}

foreach my $fn (glob "logs/*.log") {
    open my $INF, "<$fn" or die;
    my $data = "";
    while (my $line = <$INF>) {
        $data .= $line;
    }
    close $INF;
    my $line = classify($data);
    $results{$line}++;
    my $bcfn = $fn;
    die unless ($bcfn =~ s/\.log$//);
    die unless ($bcfn =~ s/^logs\///);
    $logs{$bcfn} = $fn;
    my @l;
    if (exists($triggers{$line})) {
        @l = @{$triggers{$line}};
    } else {
        @l = ();
    }
    push @l, $bcfn;
    $triggers{$line} = \@l;
}

sub bynum {
    return $results{$a} <=> $results{$b};
}

sub byvec {
    return $missing_vec{$a} <=> $missing_vec{$b};
}

my $report = "";

{
    $report .= "Missing vector instructions:\n";
    foreach my $k (sort byvec keys %missing_vec) {
        $report .= "$k : $missing_vec{$k}\n";
    }
    $report .= "\n\n";
}

my $n = 0;
my $ninsn = 0;
my $ncorrect = 0;
my $nfix = 0;
my $nunsupported = 0;
my $ntimeout = 0;
my $insn = "";
my $correct = "";
my $fix = "";
my $unsupported = "";
my $timeout = "";
my $dirnum = 0;

foreach my $k (sort bynum keys %results) {
    my $dir = "results/${dirnum}";
    ++$dirnum;
    mkdir($dir);
    my @l = @{$triggers{$k}};
    my $num = $results{$k};
    my $str = "$num : $k : [$dir]\n";

    if ($k =~ /^\[c\] /) {
        $correct .= $str;
        $ncorrect += $num;
    } elsif ($k =~ /^\[f\] /) {
        $fix .= $str;
        $nfix += $num;
    } elsif ($k =~ /^\[i\] /) {
        $insn .= $str;
        $ninsn += $num;
    } elsif ($k =~ /^\[u\] /) {
        $unsupported .= $str;
        $nunsupported += $num;
    } elsif ($k =~ /^\[t\] /) {
        $timeout .= $str;
        $ntimeout += $num;
    } else {
        die "OOPS: $str";
    }
    $n += $num;

    if (!($k =~ /^\[c\] /)) {
	foreach my $f (@l) {
	    die unless File::Copy::copy($PATH."/".$f, $dir);
	}
    }
}

$report .= $insn . "\n";
$report .= $unsupported . "\n";
$report .= $fix . "\n";
$report .= $correct . "\n";
$report .= $timeout . "\n";

my $pct1 = sprintf("%0.1f%%", ($ncorrect * 100.0) / $n);
my $pct2 = sprintf("%0.1f%%", ($ninsn * 100.0) / $n);
my $pct3 = sprintf("%0.1f%%", ($nfix * 100.0) / $n);
my $pct4 = sprintf("%0.1f%%", ($nunsupported * 100.0) / $n);
my $pct5 = sprintf("%0.1f%%", ($ntimeout * 100.0) / $n);

$report .= "$ncorrect correct ($pct1)\n";
$report .= "$ninsn missing AArch64 instructions ($pct2)\n";
$report .= "$nfix problems to address ($pct3)\n";
$report .= "$nunsupported unsupported stuff ($pct4)\n";
$report .= "$ntimeout timeouts ($pct5)\n";

open my $OUTF, ">report.txt" or die;
print $OUTF $report;
close $OUTF;

print $report;
