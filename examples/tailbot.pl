#!/usr/bin/perl -w

=head1 NAME

tailbot

=head1 DESCRIPTION

from the fbi bot by richardc, tails a file called 'logfile' to the channel
#tailbot.

=cut

use strict;
use base 'Bot::BasicBot';

sub connected {
    my $self = shift;

    # voodoo
    $self->forkit({ channel => '#tailbot',
                    run     => [ qw ( /usr/bin/tail -f logfile ) ] });
}

main->new(nick => 'tailbot',
          channels => ['#tailbot'])->run;
