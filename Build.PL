#!/usr/bin/perl
use warnings;
use strict;

use Module::Build;

Module::Build->new(
    module_name => 'Bot::BasicBot',
    license => 'perl',
    requires => {
      'Pod::Coverage'       => '0.01',
      'POE::Component::IRC' => '2',
      'POE'                 => '0.22',
      'Test::More'          => '0.01',
    },
    create_makefile_pl => 'traditional',
)->create_build_script;

