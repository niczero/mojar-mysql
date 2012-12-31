use 5.010_001;
use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
  NAME         => 'Mojar::Mysql',
  VERSION_FROM => 'lib/Mojar/Mysql.pm',
  ABSTRACT     => 'Powertools for MySQL databases',
  AUTHOR       => 'Nic Sandfield <niczero@cpan.org>',
  LICENSE      => 'artistic_2',
  META_MERGE   => {
    requires  => {
      perl => 5.010001,
      Mojolicious => 3.33,
      DBI => 1.53,
      'DBD::mysql' => 3.0008
    },
    resources => {
      homepage    => 'http://niczero.github.com/mojar-mysql',
      license     => 'http://www.opensource.org/licenses/artistic-license-2.0',
      repository  => 'http://github.com/niczero/mojar-mysql',
      bugtracker  => 'http://github.com/niczero/mojar-mysql/issues'
    },
    no_index => {directory => ['test']}
  },
  BUILD_REQUIRES => { 'Test::More' => '0' },
  CONFIGURE_REQUIRES => { 'ExtUtils::MakeMaker' => '6.30' },
  PREREQ_PM => {
    Mojolicious => 3.33,
    DBI => 1.53,
    'DBD::mysql' => 3.0008
  },
  EXE_FILES => [],
  test => { TESTS => 'test/*.t' }
);
