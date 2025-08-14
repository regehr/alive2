#!/usr/bin/perl

use warnings;
use strict;

while (my $line = <STDIN>) {
    if ($line =~ /^define (.+) \@([\.0-9a-zA-Z\_]+)\((.*)$/) {
	print "define $1 \@f($3\n";
    } else {
	print $line;
    }
}
