package TestBot;
use warnings;
use strict;
use base qw( Bot::BasicBot );
use Test::More;

sub connected {
  my $self = shift;
  ok(1, "connected");
  is( $self->nick, "basicbot_$$", "right nick" );
  
}

# ..now wait for the first tick..

sub tick {
  my $self = shift;
  ok(1, "tick");
  $self->say( channel => [ $self->channels ]->[0], body => "Hello $$" );
  exit;
}


1;
