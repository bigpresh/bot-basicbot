#!/usr/local/bin/perl -w

# tailbot
#
# from the fbi bot by richardc, tails a file called 'logfile' to the channel
# #tailbot.

use strict;
use base 'Bot::BasicBot';

sub connected {
    my $self = shift;

    $self->forkit({ channel => '#tailbot',
                    run     => [ qw ( /usr/bin/tail -f logfile ) ] });
}

main->new(nick => 'tailbot',
          channels => ['#tailbot'])->run;
