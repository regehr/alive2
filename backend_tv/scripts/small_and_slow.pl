#!/usr/bin/perl -w

use strict;
use File::Basename;

my $llvmdis = "/home/regehr/llvm-project/for-alive/bin/llvm-dis";

my @logs = glob "$ARGV[0]/*.log";

sub shuffle {
    my $array = shift;
    my $i;
    for ($i = @$array; --$i; ) {
        my $j = int rand ($i+1);
        next if $i == $j;
        @$array[$i,$j] = @$array[$j,$i];
    }
}

shuffle(\@logs);

my $n = 0;
foreach my $f (@logs) {
    open my $INF,"<$f" or die;
    my $time;
    my $func;
    while (my $line = <$INF>) {
	if ($line =~ /runtime: ([0-9\.]+)$/) {
	    $time = $1;
	}
	if ($line=~ /\'\-fn\' \'(.*?)\'/) {
	    $func = $1;
	}
    }
    close $INF;
    next unless defined $time;
    next unless defined $func;
    next unless $time >= 600.0;
    my $bcfile = $f;
    my $base = File::Basename::basename($f);
    die unless $base =~ s/_[0-9]+\.log$/.bc/g;
    open my $I2, "$llvmdis /home/regehr/spec-funcs/$base -o - |" or die;
    my $body;
    while (my $line = <$I2>) {
	if ($line =~ /^define.*@($func)\(.*\{/) {
	    $body = $line;
	    last;
	}	
    }
    my $pointermem = 0;
    while (my $line = <$I2>) {
	chomp $line;
	$body .= "$line\n";
	if ($line =~ /load ptr/ ||
	    $line =~ /store ptr/) {
	    $pointermem = 1;
	}
	last if ($line =~ /^\}$/);
    }
    close $I2;
    my $size = length($body);
    die unless $size > 0;
    next if ($size > 300);
    next if ($pointermem);
    print "$f\n";
    print "    $func\n";
    print "    size = $size\n";
    print "\n$body\n\n";
}

