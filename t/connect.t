#!perl
use warnings;
use strict;
use Test::More;

use lib qw(lib t/lib);

require IO::Socket;
my $s = IO::Socket::INET->new(
  PeerAddr => "irc.perl.org:6667",
  Timeout  => 10,
);

if ($s) {
  close($s);
  plan tests => 3;
} else {
  plan skip_all => "no net connection available";
  exit;
}



use TestBot;

my $bot = TestBot->new(
  nick => "basicbot_$$",
  server => "london.irc.perl.org",
  channels => ["#bot_basicbot_test"],
);

$bot->run;

