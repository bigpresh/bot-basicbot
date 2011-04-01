use Test::More;
eval "use Test::Pod::Coverage 1.00";
plan skip_all => "Test::Pod::Coverage 1.00 required for testing POD coverage" if $@;

my $opts = { also_private => [ qr/^\w+_state$/] };

my @modules = all_modules();

plan tests => scalar @modules;

pod_coverage_ok($_, $opts) for @modules;
