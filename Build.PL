#!/usr/bin/perl
use warnings;
use strict;

use Module::Build;

Module::Build->new(
    module_name => 'Bot::BasicBot',
    license => 'perl',
    requires => {
      'POE::Component::IRC' => '2',
      'POE'                 => '0.22',
      'Test::More'          => '0.01',
      'Text::Wrap'          => 0,
    },
    create_makefile_pl => 'passthrough',
    create_readme => 1,
)->create_build_script;

