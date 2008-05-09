#!/usr/bin/perl

use warnings;
use strict;

my $text;
my $regex;
my $match;
my $start;
my $length;

my $line;
my $text_line;
my $regex_line;

while (<>) {
        $line ++;
	if (/^Text: (.*)$/) {
		$text = $1;
                $text_line = $line;
	} elsif (/^Regex: (.*)$/) {
		$regex = $1;
                $regex_line = $line;
	} elsif ( (/^(Match: T 0s: \d+ 0l: \d+)\s+$/) or
		  (/^(Match: F)\s+$/) ) {
		my $expected = $1;
		my $actual = &runtest($text, $regex);
		if ($expected eq $actual) {
			print "$line: Ok\n";
		} else {
			print "$line: Error: with text '$text' (line $text_line)  and regex '$regex' (line $regex_line). I expected: '$expected' but got: '$actual'\n";
			&runtestDebug($text, $regex);
		}
	}
}

sub runtest {
	my ($text, $regex) = @_;
	my $result = `./regex_test '$regex' '$text'`;
	chomp($result);
	$result =~ s/^\s+(.*)/$1/;
	return $result;
}
sub runtestDebug {
	my ($text, $regex) = @_;
	my $result = `./regex_test '$regex' '$text' debug`;
	chomp($result);
	$result =~ s/^\s+(.*)/$1/;
	return $result;
}
