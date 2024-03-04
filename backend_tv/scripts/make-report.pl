#!/usr/bin/perl

use warnings;
use strict;
use File::Copy;

my %results = ();
my %logs = ();
my %missing_vec = ();

my $CORRECT = "Translation validated as correct";

sub classify($) {
    (my $ref) = @_;
    my @lines = @{$ref};

    foreach my $data (@lines) {
        if ($data =~ /seems to be correct/) {
            return "[c] $CORRECT";
        }
        if ($data =~ /sending signal TERM to command/) {
            return "[u] Timeout";
        }
        if ($data =~ /^(ERROR: volatiles not supported)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Out of memory; skipping function.)/) {
            return "[u] $1";
        }
        if ($data =~ /^ERROR: va_arg instructions not supported/) {
            return "[u] ERROR: varargs not supported";
        }
        if ($data =~ /^ERROR: Timeout/) {
            return "[u] Timeout";
        }
        if ($data =~ /^(LLVM ERROR: .*)$/) {
            return "[f] $1";
        }
        if ($data =~ /^(Segmentation fault)/) {
            return "[f] $1";
        }
        if ($data =~ /^(ERROR: Unsupported AArch64 instruction.*)/) {
            return "[i] $1";
        }
        if ($data =~ /^(ERROR: Function is too large)/) {
            return "[u] $1";
        }
        if ($data =~ /^(LLVM ERROR: Cannot select.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Unsupported metadata.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Source function is always UB.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Too many temporaries.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: personality functions not supported.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: NZCV is the only supported case for MSR.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: NZCV is the only supported case for MRS.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: SMT Error.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Unsupported instruction)/) {
            return "[u] ERROR: LLVM instruction not supported by Alive2";
        }
        if ($data =~ /^(ERROR: The source program doesn't reach a return instruction.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Unsupported type.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Only the C calling convention is supported)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: varargs not supported.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Unsupported attribute.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Could not translate.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^(ERROR: Not supporting.*)/) {
            return "[u] $1";
        }
        if ($data =~ /^ERROR: UB triggered/) {
            return "[f] UB triggered";
        }
        if ($data =~ /^ERROR: Value mismatch/) {
            return "[f] Value mismatch";
        }
        if ($data =~ /^(error: .*)$/) {
            return "[f] $1";
        }
        if ($data =~ /^(ERROR: .*)$/) {
            return "[f] $1";
        }
        if ($data =~ /\/(lifter.cpp:[0-9]+):.*Assertion `/) {
            return "[f] Assertion failed at $1";
        }
        if ($data =~ /\/(Instructions.cpp:[0-9]+):.*Assertion `/) {
            return "[f] Assertion failed at $1";
        }
        if ($data =~ /\/(Instructions.cpp:[0-9]+):.*Assertion `/) {
            return "[f] Assertion failed at $1";
        }
        if ($data =~ /\/(MCInst.h:[0-9]+):.*Assertion `/) {
            return "[f] Assertion failed at $1";
        }
        if ($data =~ /^(UNREACHABLE.*)$/) {
            return "[f] $1";
        }
        if ($data =~ /Aslp assertion failure! (.*)$/) {
            return "[f] Aslp: $1";
        }
    }
    return "[f] Unknown failure";
}

my %cmds = ();

foreach my $fn (glob "logs/*.log") {
    # print "$fn\n";
    open my $INF, "<$fn" or die;
    my @lines = ();
    my $cmdline;
    while (my $line = <$INF>) {
        $cmdline = $line unless defined($cmdline);
        push @lines, $line;
    }
    close $INF;
    my $line = classify(\@lines);
    $results{$line}++;
    $cmds{$line} .= $cmdline;
}

sub bynum {
    return $results{$a} <=> $results{$b};
}

sub byvec {
    return $missing_vec{$a} <=> $missing_vec{$b};
}

my $report = "";

my $n = 0;
my $ninsn = 0;
my $ncorrect = 0;
my $nfix = 0;
my $nunsupported = 0;
my $insn = "";
my $correct = "";
my $fix = "";
my $unsupported = "";
my $counter = 0;

foreach my $k (sort bynum keys %results) {
    print "$k\n";
    my $cmdfn = "commands_${counter}.txt";
    $counter++;
    open my $CMDS, ">$cmdfn" or die;
    print $CMDS $cmds{$k};
    close $CMDS;
    my $num = $results{$k};
    my $str = "$num ($cmdfn) : $k\n";

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
    } else {
        die "OOPS: $str";
    }
    $n += $num;
}

$report .= $insn . "\n";
$report .= $unsupported . "\n";
$report .= $fix . "\n";
$report .= $correct . "\n";

my $pct1 = sprintf("%0.1f%%", ($ncorrect * 100.0) / $n);
my $pct2 = sprintf("%0.1f%%", ($ninsn * 100.0) / $n);
my $pct3 = sprintf("%0.1f%%", ($nfix * 100.0) / $n);
my $pct4 = sprintf("%0.1f%%", ($nunsupported * 100.0) / $n);

$report .= "$ncorrect correct ($pct1)\n";
$report .= "$ninsn missing AArch64 instructions ($pct2)\n";
$report .= "$nfix problems to address ($pct3)\n";
$report .= "$nunsupported unsupported stuff ($pct4)\n";

open my $OUTF, ">report.txt" or die;
print $OUTF $report;
close $OUTF;

print $report;
