#!/usr/bin/perl
use warnings;
use strict;
use lib qw(./lib);
use Pod::Coverage;

use Test::More no_plan => 1;

use_ok('Bot::BasicBot');

ok(my $pc = Pod::Coverage->new( package => 'Bot::BasicBot' ), "created coverage object");

my $cover = $pc->coverage;
my $uncovered = join(", ", $pc->naked);

ok($cover == 1, "full coverage".($uncovered ? " (uncovered: $uncovered)" : ""));
