# ============
# mysql-helper
# ============
package T::MysqlHelperA;
use Mojo::Base -base;

use Mojar::Mysql::Connector (
  cnf => 'data/tester_localhost',
  RaiseError => 0,
  AutoCommit => 0,
  -dbh => 1
);

package main;
use Test::More;

my $mha;

subtest q{Basics} => sub {
  can_ok('T::MysqlHelperA', qw(new connector dbh)), 'expected methods';
};

subtest q{connector} => sub {
  ok $mha = T::MysqlHelperA->new, 'new()';
  ok my $connector = $mha->connector, 'connector()';
  isa_ok $connector, 'Mojar::Mysql::Connector', 'connector isa ::Connector';
  ok $connector->can('connect'), 'and can connect()';

  is $connector->{RaiseError}, 0, 'contains correct hash value (RaiseError)';
  is $connector->{AutoCommit}, 0, 'contains correct hash value (AutoCommit)';
};

SKIP: {
  skip 'set TEST_MYSQL to enable this test (developer only!)', 1
    unless $ENV{TEST_MYSQL} || $ENV{TRAVIS};

subtest q{dbh} => sub {
  ok my $dbh = $mha->dbh, 'dbh()';
  isa_ok $dbh, 'Mojar::Mysql::Connector::db', 'dbh isa ::Connector::db';
  ok $dbh->can('ping'), 'and can ping()';
};
};

done_testing();
