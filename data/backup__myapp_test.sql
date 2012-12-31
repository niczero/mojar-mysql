DROP DATABASE IF EXISTS myapp_test;
CREATE DATABASE myapp_test
  DEFAULT CHARACTER SET utf8 DEFAULT COLLATE utf8_general_ci;
USE myapp_test;

DROP TABLE IF EXISTS foo;
CREATE TABLE foo (
  f0 int unsigned NOT NULL auto_increment,
  f1 varchar(255),
  f2 timestamp NOT NULL,
  PRIMARY KEY (f0),
  UNIQUE KEY (f1)
) ENGINE=InnoDB comment='Test table foo';

INSERT INTO foo (f1)
VALUES
  ('Lorem ipsum'), ('dolor sit amet'), ('consectetur adipisicing elit'),
  ('sed do eiusmod tempor incididunt'), ('ut labore et dolore magna aliqua.'),
  ('Ut enim ad minim veniam,'), (' quis nostrud exercitation ullamco laboris'),
  ('nisi ut aliquip ex ea commodo consequat.'), ('Duis aute irure dolor in'),
  ('reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla'),
  ('pariatur.');
