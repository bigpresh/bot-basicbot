#!/usr/bin/perl
use warnings;
use strict;

use Module::Build;

Module::Build->new(
    module_name => 'Bot::BasicBot',
    license => 'perl',
    requires => {
        'perl'        => '5.6.0',
        'POE'         => '0.22',
        'POE::Component::IRC' => '2',
        'Test::More' => 0,
        'Pod::Coverage' => 0,
    },
    create_makefile_pl => 'traditional',
)->create_build_script;

