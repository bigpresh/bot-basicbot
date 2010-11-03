#!/usr/bin/perl
use warnings;
use strict;
use lib qw(./lib);

use Test::More tests => 1;

use_ok('Bot::BasicBot');

# Yes, I _know_. Tell you what, _you_ come up with a good way to test an IRC
# bot, and tell me, and I'll write some tests.
