=head1 NAME

namer - read out url titles in the channel

=cut

#!/usr/bin/perl
package Bot;
use base qw(Bot::BasicBot);
use warnings;
use strict;
use URI::Title;

sub said {
  my $self = shift;
  my $message = shift;
  my $body = $message->{body};
  return unless my @urls = find_urls($message->{body});
  $self->reply($message, title($_)) for (@urls);
}

# this function could be _so_ much smarter.
sub find_urls {
  my $text = shift;
  return ($1) if ($text =~ m!(http://\S+)!);
  return undef;
}

package main;

Bot->new(
  channels => [ '#jerakeen' ],
  nick => 'namer',
)->run();

