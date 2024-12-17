#!/usr/bin/perl

use warnings;
use strict;

my %results = ();
my %logs = ();
my %missing_vec = ();

my $CORRECT = "Translation validated as correct";
my $UNKNOWN = "[f] Unknown failure";

sub classify($) {
    (my $ref) = @_;
    my @lines = @{$ref};

    foreach my $data (reverse @lines) {
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
        if ($data =~ /(Assertion `.*)$/) {
            return "[f] $1";
        }
        if ($data =~ /^(OOPS: .*)$/) {
            return "[f] $1";
        }
        if ($data =~ /^(UNREACHABLE.*)$/) {
            return "[f] $1";
        }
        if ($data =~ /Aslp assertion failure! (.*)$/) {
            return "[f] Aslp: $1";
        }
        if ($data =~ /^[^:]+:\d+:\d+: (runtime error: .*)$/) {
            return "[f] $1";
        }
        if ($data =~ /^SUMMARY: AddressSanitizer: stack-overflow/) {
            return "[u] stack-overflow";
        }
        if ($data =~ /^SUMMARY: AddressSanitizer: (.*)$/) {
            return "[u] AddressSanitizer $1";
        }
        if ($data =~ /^(malloc\(\): unaligned tcache chunk detected)/) {
            return "[f] $1";
        }
        if ($data =~ /^(free\(\): invalid pointer)/) {
            return "[f] $1";
        }

    }
    return $UNKNOWN;
}

my $dir = $ARGV[0];
die "please specify directory of logs" unless (-d $dir);
my @files = glob "$dir/*.log";
print STDERR "found ", scalar @files, " .log files in $dir\n";

foreach my $f (@files) {
    open my $INF, "<$f" or die "reading $f failed";
    my @lines = ();
    my $cmdline;
    while (my $line = <$INF>) {
        $cmdline = $line unless defined($cmdline);
        push @lines, $line;
    }
    close $INF;
    my $cls = classify(\@lines);
    print "$f|";

    if ($cls eq $UNKNOWN) {
        print "unknown";
    } else {
        print "$cls";
    }

    if ($lines[-1] =~ /runtime: (\d+)/) {
        print "|$1";
    } else {
        print "|";
    }


    print "\n";
}
