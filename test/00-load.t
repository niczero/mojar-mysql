use Mojo::Base -strict;
use Test::More;

use_ok 'Mojar::Mysql';
diag "Testing Mojar::Mysql $Mojar::Mysql::VERSION, Perl $], $^X";
use_ok 'Mojar::Mysql::Connector';

done_testing();
