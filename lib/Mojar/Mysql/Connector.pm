# ============
package Mojar::Mysql::Connector;
# ============
use DBI 1.4.3;
use Mojo::Base 'DBI';
# Register subclass structure
__PACKAGE__->init_rootclass;

our $VERSION = 2.101;

use File::Spec::Functions 'catfile';

sub import {
  my ($pkg, %param) = @_;
  my $caller = caller;
  # Helpers
  $param{-connector} //= 1 if exists $param{-dbh} && $param{-dbh};
  if (exists $param{-connector} and my $cname = delete $param{-connector}) {
    $cname = 'connector' if "$cname" eq '1';
    no strict 'refs';
    *{"${caller}::$cname"} = sub {
      my $self = shift;
      if (@_) {
        $self->{$cname} = (@_ > 1) ? Mojar::Mysql::Connector->new(@_) : shift;
        return $self;
      }
      return $self->{$cname} //= Mojar::Mysql::Connector->new;
    };
    if (exists $param{-dbh} and my $hname = delete $param{-dbh}) {
      $hname = 'dbh' if "$hname" eq '1';
      *{"${caller}::$hname"} = sub {
        my $self = shift;
        if (@_) {
          $self->{$hname} = (@_ > 1) ? $self->$cname->connect(@_) : shift;
          return $self;
        }
        return $self->{$hname}
          if defined $self->{$hname} && $self->{$hname}->ping;
        return $self->{$hname} = $self->$cname->connect;
      };
    }
  }
  # Global defaults
  if (%param and %{$pkg->Defaults}) {
    # Already have defaults => check unchanged
    my $ps = join ':', map +($_ .':'. ($param{$_} // 'undef')),
        sort keys %param;
    my $ds = join ':', map +($_ .':'. ($pkg->Defaults->{$_} // 'undef')),
        sort keys %{$pkg->Defaults};
    die "Redefining class defaults for $pkg" unless $ps eq $ds;
  }
  @{ $pkg->Defaults }{keys %param} = values %param if %param;
  # Debugging
  $pkg->trace($param{TraceLevel})
    if exists $param{TraceLevel} && defined $param{TraceLevel};
}

# ------------
# Class attributes
# ------------


# ------------
# Attributes
# ------------

my @DbdFields = qw( RaiseError PrintError PrintWarn AutoCommit TraceLevel
    mysql_enable_utf8 mysql_auto_reconnect );

has RaiseError => 1;
has PrintError => 0;
has PrintWarn => 0;
has AutoCommit => 1;
has TraceLevel => 0;
has mysql_enable_utf8 => 1;
has mysql_auto_reconnect => 0;

my @ConFields = qw( label cnfdir cnf cnfgroup );

has 'label';
has cnfdir => '.';
has 'cnf';
has 'cnfgroup';

my @DbiFields = qw( driver host port schema user password );

has driver => 'mysql';
has 'host';  # eg 'localhost'
has 'port';  # eg 3306
has 'schema';  # eg 'test';
has 'user';
has 'password';

# Private function

sub croak { require Carp; goto &Carp::croak; }

# ------------
# Public methods
# ------------

sub new {
  my ($proto, %param) = @_;
  # $proto may contain defaults to be cloned
  # %param may contain defaults for overriding
  my %defaults = ref $proto
      ? ( %{ ref($proto)->Defaults }, %$proto ) : %{ $proto->Defaults };
  return Mojo::Base::new($proto, %defaults, %param);
}

sub connect {
  my ($proto, @args) = @_;
  my $class = ref $proto || $proto;
  @args = $proto->dsn(@args)
    unless @args && $args[0] =~ /^DBI:/i;
  my $dbh;
  eval {
    $dbh = $class->SUPER::connect(@args)
  }
  or do {
    my $e = $@;
    croak sprintf "Connection error\n%s\n%s",
        $proto->dsn_as_string(@args), $e;
  };
  return $dbh;
}

sub dsn {
  my ($proto, %param) = @_;

  # Derive dynamic defaults from object or class
  my %defaults = ref $proto ? ( %{ ref($proto)->Defaults }, %$proto )
                            : %{ $proto->Defaults };
  # Absorb dynamic defaults
  %param = ( %defaults, %param ) if %defaults;
  # Fallback to static defaults
  for (@ConFields, @DbiFields, @DbdFields) {
    $param{$_} = $proto->$_ unless exists $param{$_};
  }

  my $cnf_txt = '';
  if (my $cnf = $param{cnf}) {
    # MySQL .cnf file
    $cnf .= '.cnf' unless $cnf =~ /\.cnf$/;
    unless (-r $cnf) {
      $cnf = catfile $param{cnfdir}, $cnf if defined $param{cnfdir};
    }
    croak sprintf 'Failed to read .cnf file (%s)', $cnf unless -r $cnf;

    $cnf_txt = ';mysql_read_default_file='. $cnf;
    $cnf_txt .= ';mysql_read_default_group='. $param{cnfgroup}
      if defined $param{cnfgroup};
  }

  # DBD params
  # Only set private_config if it would have meaningful values
  my %custom;
  for (qw(label cnf cnfgroup)) {
    $custom{$_} = $param{$_} if defined $param{$_};
  }
  my $dbd_param = %custom ? { private_config => {%custom} } : {};
  @$dbd_param{@DbdFields} = @param{@DbdFields};

  return (
    'DBI:'. $param{driver} .q{:}
          . ($param{schema} // $param{db} // '')
          . (defined $param{host} ? q{;host=}. $param{host} : '')
          . (defined $param{port} ? q{;port=}. $param{port} : '')
          . $cnf_txt,
    $param{user},
    $param{password},
    $dbd_param
  );
}

sub dsn_as_string {
  my ($proto, @args) = @_;
  # Password
  if ($args[2] and $_ = length $args[2] and $_ > 1) {
    --$_;
    my $blanks = '*' x $_;
    $args[2] = substr($args[2], 0, 1). $blanks;
  }
  require Data::Dumper;
  my $s = Data::Dumper::Dumper([@args]);
  $s =~ s/^\$VAR1 /dsn /;
  $s =~ s/^\s+]/]/m;
  $s =~ s/\n\z//;
  return $s;
}

{
  my $_Defaults = {};

  sub Defaults {
    return $_Defaults if @_ == 1;
    $_Defaults = $_[1];
    $_[0];
  }
}

# ============
package Mojar::Mysql::Connector::db;
# ============
@Mojar::Mysql::Connector::db::ISA = 'DBI::db';

use Mojar::Util 'lc_keys';
use Scalar::Util 'looks_like_number';

# Private functions

sub croak { require Carp; goto &Carp::croak; }

our $_as_hash = { Slice => {} };
sub as_hash { $_as_hash }

# ------------
# Public methods
# ------------

sub mysqld_version { shift->get_info(18) }
# 18 : SQL_DBMS_VER

sub thread_id { shift->{mysql_thread_id} // 0 }

sub current_schema {
  my ($self) = @_;
  my $schema_name;
  eval {
    ($schema_name) = $self->selectrow_array(
q{SELECT DATABASE()});
    1;
  }
  or do {
    my $e = $@ // '';
    croak "Failed to identify schema name\n$e";
  };
  return $schema_name;
}

sub session_var {
  my ($self, $var, $value) = (shift, shift, undef);
  croak 'Missing var name' unless defined $var && length $var;
  unless (@_) {
    eval {
      ($value) = $self->selectrow_array(sprintf
q{SELECT @@session.%s}, $var);
      1;
    }
    or do {
      my $e = $@ // '';
      croak "Failed to get var ($var)\n$e";
    };
    return $value;
  }

  $value = shift;
  my ($old, $new);
  eval {
    ($old) = $self->selectrow_array( sprintf
q{SELECT @@session.%s}, $var);
    $value = sprintf "'%s'", $value unless looks_like_number $value;
    $self->do(qq{SET SESSION $var = $value});
    ($new) = $self->selectrow_array(sprintf
q{SELECT @@session.%s}, $var);
    1;
  }
  or do {
    my $e = $@ // '';
    croak "Failed to set var ($var)\n$e";
  };
  return wantarray ? ($old, $new) : $self;
}

sub disable_quotes { shift->session_var( sql_quote_show_create => 0 ) }

sub enable_quotes {
  my ($self, $value) = @_;
  $value //= 1;
  $self->session_var( sql_quote_show_create => $value )
}

sub disable_fk_checks { shift->session_var( foreign_key_checks => 0 ) }

sub enable_fk_checks {
  my ($self, $value) = @_;
  $value //= 1;
  $self->session_var( foreign_key_checks => $value )
}

sub schemata {
  my ($self, @args) = @_;
  # args[0] : schema pattern
  my $schemata;
  eval {
    my $sql = q{SHOW DATABASES};
    $sql .= sprintf q{ LIKE '%s'}, $args[0] if defined $args[0];
    $schemata = $self->selectcol_arrayref($sql, $args[1]);
    @$schemata = grep !/^lost\+found/, @$schemata;
    1;
  }
  or do {
    croak "Failed to list schemata\n$@";
  };
  return $schemata;
}

sub tables_and_views {
  my ($self, @args) = @_;
  # args[0] : schema
  # args[1] : table pattern
  # args[2] : type
  # args[3] : attr
  $args[2] //= 'TABLE,VIEW';
  my $tables;
  eval {
    my $sth = $self->table_info('', @args);
    @$tables = map $_->[2], @{$sth->fetchall_arrayref};
    1;
  }
  or do {
    croak "Failed to list tables\n$@";
  };
  return $tables;
}

sub real_tables {
  my ($self, @args) = @_;
  # args[0] : schema
  # args[1] : table pattern
  # args[2] : attr
  return $self->tables_and_views(@args[0,1], 'TABLE', $args[2]);
}

sub views {
  my ($self, @args) = @_;
  # args[0] : schema
  # args[1] : table pattern
  # args[2] : attr
  return $self->tables_and_views(@args[0,1], 'VIEW', $args[2]);
}

sub selectall_hashref {
  my ($self, $sql, $opts, @args) = @_;
  if (defined $opts) {
    $opts->{Slice} = {};
  }
  else {
    $opts = $_as_hash;
  }
  return $self->selectall_arrayref($sql, $opts, @args);
}

sub selectrow_hashref { shift->selectall_hashref(@_)->[0] }

sub engines {
  my ($self) = @_;

  my $engines = {};
  my $e = $self->selectall_arrayref(q{SHOW ENGINES});
  for (@$e) {
    if ($_->[1] eq 'DEFAULT') {
      $engines->{default} = lc $_->[0];
      $engines->{lc $_->[0]} = 1;
    }
    else {
      $engines->{lc $_->[0]} = $_->[1] eq 'YES' ? 1 : 0;
    }
  }
  return $engines;
}

sub statistics {
  my ($self) = @_;

  # Arbitrary query to ensure results
  ($_) = $self->selectrow_array(q{SELECT VERSION()});

  my $s = $self->selectall_arrayref(q{SHOW /*!50000 GLOBAL */ STATUS});
  return { map @$_, @$s };
}

sub variables {
  my ($self) = @_;

  my $v = $self->selectall_arrayref(q{SHOW /*!50000 GLOBAL */ VARIABLES});
  my $variables = { map @$_, @$v };

  # Workaround for MySQL bug #59393 wrt ignore-builtin-innodb
  $variables->{have_innodb} = 'NO'
    if exists $variables->{ignore_builtin_innodb}
        && ($variables->{ignore_builtin_innodb} // '') eq 'ON';

  return $variables;
}

sub indices {
  my ($self, $schema, $table) = @_;
  croak 'Missing required schema name' unless defined $schema and length $schema;
  croak 'Missing required table name'  unless defined $table and length $table;
  my $i = $self->selectall_arrayref(sprintf(
q{SHOW INDEXES FROM %s IN %s}, $table, $schema
    ), $_as_hash
  );
  # $i is arrayref of hashrefs
  lc_keys $_ for @$i;
  return $i;
}

sub table_status {
  my ($self, $schema, $table_pattern) = @_;
  croak 'Missing required schema name' unless defined $schema and length $schema;
  my $sql = sprintf
q{SHOW TABLE STATUS FROM %s}, $schema;
  $sql .= sprintf(q{ LIKE '%s'}, $table_pattern) if defined $table_pattern;
  my $s = $self->selectall_arrayref($sql, $_as_hash);
  # $s is arrayref of hashrefs
  lc_keys $_ for @$s;
  return $s;
}

sub engine_status {
  my ($self, $engine) = @_;
  $engine //= 'InnoDB';

  my ($raw) = $self->selectrow_array(
q{SHOW INNODB STATUS}
  );

  my ($title, $buffer) = ('', '');
  my $status = {};
  for (split /^/, $raw) {
    if (/^\-+$/ and length $buffer) {
      # Finish previous record
      $status->{$title} = $buffer;
      $title = $buffer = '';
    }
    elsif (/^-+$/) {
      # Start new record
    }
    elsif (not length $title) {
      chomp;
      $title = lc $_;
      $title =~ s/\s/_/g;
      $title =~ s/\W//g;
    }
    else {
      $buffer .= $_;
    }
    # Ignore final record
  }
  return $status;
}

sub thread_list {
  my ($self) = @_;
  my $tl = $self->selectall_arrayref(
q{SHOW FULL PROCESSLIST}, $_as_hash
  );
  for my $a (@$tl) {
    %$a = map +(lc($_) => $a->{$_}), keys %$a;
  }
  return $tl;
}

sub table_space {
  my ($self, $schema, $table) = @_;
  my $space;
  eval {
    ($space) = $self->selectrow_array(
q{SELECT CONCAT(TRUNCATE(DATA_FREE / 1024, 0), ' kB')
FROM information_schema.TABLES
WHERE
  TABLE_SCHEMA = ?
  AND TABLE_NAME = ?},
      undef,
      $schema, $table
    );
    $space ne '0 kB';
  }
  or eval {
    my $comment = $self->table_status($schema, $table)->[0]{comment};
    $space = $1 if $comment =~ /InnoDB free: (\d+ \w+)/;
  };
  return $space;
}

sub date_from_today {
  my ($self, $days, $format) = @_;
  $days //= 0;
  $format //= '%Y-%m-%d';
  my ($date) = $self->selectrow_array(sprintf
q{SELECT DATE_FORMAT(DATE_ADD(CURDATE(), INTERVAL %s DAY), '%s')},
    $days,
    $format
  );
  return $date;
}

#TODO: clean up this old code
#sub insert_hash {
#  my ($self, $schema, $table, $field_map) = @_;
#  my @fields = keys %$field_map;
#  my @values = values %$field_map;
#  my $sql = sprintf
#q{INSERT INTO %s.`%s` (%s) VALUES (%s)},
#      $schema,
#      $table,
#      join(q{,}, @fields),
#      join(q{,}, '?' x @fields);
#  my $sth = $self->prepare($sql);
#  $sth->execute(@values)
#}

#TODO: clean up this old code
#sub search_hash {
#  my ($self, $schema, $table, $field_map, @columns) = @_;
#  my @fields = keys %$field_map;
#  my @values = values %$field_map;
#  my $clause = '';
#  $clause = q{WHERE }. join q{ AND }, map '$_ = ?', @fields if @fields;
#  my $wanted = scalar(@columns) ? join q{, }, @columns : q{*};
#  my $sth = $self->prepare( sprintf
#q{SELECT %s FROM %s.%s %s}, $wanted, $schema, $table, $clause);
#  $self->selectall_arrayref($sth, {}, @values)
#}

# ============
package Mojar::Mysql::Connector::st;
# ============
@Mojar::Mysql::Connector::st::ISA = 'DBI::st';

1;
__END__

=head1 NAME

Mojar::Mysql::Connector - MySQL connector (dbh producer) with added convenience

=head1 SYNOPSIS

In an application making only one type of connection.

  use Mojar::Mysql::Connector (
    cnfdir => '/var/local/auth/myapp',
    cnf => 'rw_localhost',
    schema => 'Users'
  );
  ...
  my $dbh = Mojar::Mysql::Connector->connect;

In an application making multiple types of connection.

  use Mojar::Mysql::Connector (
    cnfdir => '/var/local/auth/myapp'
  );

  my $read_connector = Mojar::Mysql::Connector->new(
    cnf => 'ro_remotehost',
    schema => 'Orders'
  );
  my $write_connector = Mojar::Mysql::Connector->new(
    cnf => 'rw_localhost',
    schema => 'Reports'
  );
  ...
  $read_connector->connect->do(q{...});
  ...
  my $read_dbh = $read_connector->connect(auto_reconnect => 1);
  my $write_dbh = $write_connector->connect;

Employing a helper.

  use Mojar::Mysql::Connector (
    cnfdir => '/var/local/auth/myapp',
    cnf => 'rw_localhost',
    schema => 'Users',
    -dbh => 1
  );
  sub do_da_db_doodah {
    my $self = shift;
    my $dbh = $self->dbh;
    ...
  }

From the commandline.

  perl -MMojar::Mysql::Connector="cnf,ro_localhost,schema,Users,-dbh,1"
    -E'say join qq{\n}, @{main->dbh->real_tables}'

=head1 DESCRIPTION

MySQL-specific extension (subclass) to DBI in order to improve convenience,
security, and error handling.  Supports easy use of credential (cnf) files, akin
to

  mysql --defaults-file=$CRED_FILE

It aims to reduce boilerplate, verbosity, mistakes, and parameter overload, but
above all it tries to make it quick and easy to Do The Right Thing.

As the name implies, the class provides connector objects -- containers for
storing and updating your connection parameters.  When you call C<connect>, the
connector returns a handle created using its retained parameters plus any
call-time parameters passed.  You don't however have to use connectors; for
simple usage it can be easier to use C<connect> directly from the class.

You can use a DSN tuple if you want to, but it's more readable and less
error-prone to specify your parameters either as a hash or by setting individual
attributes.  Each call to C<connect> will then construct the DSN for you.

You can optionally import a helper method, called C<dbh> or whatever name you
choose, so that you can focus even less on the connector/connection and more on
your code.  The helper will cache your database handle and create a new one
automatically if the old one is destroyed or goes stale.

The fourth layer of convenience is provided by the added database handle
methods.  Changing session variables is as easy as chaining methods, listing
only genuine tables (C<real_tables>) is easy, and there's more.

=head1 USAGE

The class is designed to be used in several different ways and what works best
for your current project will depend on factors such as the runtime environment
(mod_perl vs hypnotoad vs single script vs ...), the other code you are using
(Moose vs Mojo vs bespoke vs ...), and of course your (team's) taste.

=head2 Using C<use> parameters

This works well in an isolated script; simply put everything in your C<use>
statement then call C<connect> as a class method.

  #!/usr/bin/env perl
  use Mojar::Mysql::Connector (
    cnf    => 'myuser_localhost',
    schema => 'Stats'
  );
  my $dbh = Mojar::Mysql::Connector->connect;
  $dbh->do(q{...});
  my @result = $dbh->selectrow_array(q{...});
  $dbh->disconnect;

If you have a class with hashref-based objects, you might prefer the convenience
of the helper C<dbh>.

  #!/usr/bin/env perl
  package Worker;
  use Mojar::Mysql::Connector (
    cnf    => 'myuser_localhost',
    schema => 'Stats',
    -dbh => 1
  );
  # Worker now has attributes 'connector' & 'dbh'
  sub new {
    my $class = shift;
    return bless {} => $class;
  }
  sub perform {
    my $self = shift;
    my $dbh = $self->dbh;
    # dbh was cached then checked before delivery
    # Can leave dbh connected
  }

  package main;
  Worker->new->perform;

Not as elegant, but technically it works without a class too.

  #!/usr/bin/env perl
  use Mojar::Mysql::Connector (
    cnf    => 'myuser_localhost',
    schema => 'Stats',
    -dbh => 1
  );
  # 'main' now has attributes 'connector' & 'dbh'
  sub perform {
    my $dbh = main->dbh;
    # dbh was cached then checked before delivery
    # Can leave dbh connected
  }

  perform();

The same approach works well from the commandline.

  perl -MMojar::Mysql::Connector="cnf,ro_localhost,-dbh,1"
    -E'say sprintf q{Server version is %s}, main->dbh->mysqld_version'

In a shared environment like C<mod_perl> it is unsafe to use class attributes
unless you can guarantee all code and future code will use identical use-time
parameters.  In those environments it is safer to use a connector.

=head2 Using connectors

The usage is very similar but with the database parameters packed into an object
(connector) rather than the class.

  package C::ResponsibleSharer;
  use Mojar::Mysql::Connector;

  my $connector = Mojar::Mysql::Connector->new(
    cnfdir => '/srv/myapp/data',
    cnf => 'ro_localhost',
    schema => 'Users'
  );

  sub do_something {
    my $dbh = $connector->dbh;  # $dbh is created fresh each call
    $dbh->do(q{...});
    my ($user_id) = $dbh->selectrow_array(q{SELECT ...});
  }

For better performance you may prefer to do that via helpers which will cache
the database handle and regenerate it only when necessary.

=head2 Using helpers

  package C::ResponsibleSharer;
  use Mojar::Mysql::Connector(-dbh => 1);
  # ->dbh helper requires an implicit ->connector helper

  sub init_once {
    my $self = shift;
    $self->connector->cnfdir('/srv/myapp/data')
        ->cnf('ro_localhost')
        ->schema('Users');
  }

  sub do_something {
    my $self = shift;
    my $dbh = $self->dbh;  # $dbh is cached and recreated when necessary
    $dbh->do(q{...});
    my ($user_id) = $dbh->selectrow_array(q{SELECT ...});
  }

Using the helpers entails having object hashref keys C<connector> and C<dbh> as
well as methods C<-E<gt>connector> and C<-E<gt>dbh>.  It is possible these might
clash with identifiers you already have, in which case you can customise your
helpers.

=head3 Customising your helpers

You can even choose the names of your helpers.

  package C::FussyNamer;
  use Mojar::Mysql::Connector(
    cnfdir => '/srv/myapp/data',
    cnf => 'ro_localhost',
    schema => 'Users',
    -connector => 'connecteur',
    -dbh => 'poignee'
  );
  # methods ->connecteur and ->poignee now exist
  my $obj = C::FussyNamer->new;
  # the helpers are lazy so nothing additional has been created yet
  my $dbh = $obj->poignee;
  # $obj->{poignee} has been vivified, which in turn vivified $obj->{connecteur}

=head1 ATTRIBUTES

A main motivator for this class is the easy management of connection parameters
via attributes.  We've already seen setting attributes in the C<use> statement,
on a connector both in a creation hash and via methods, in a call to C<connect>,
and via helpers.  We've seen inheriting attributes when cloning a connector from
an existing connector and overriding attributes at time of creation/connection.
Now let's look at the attributes themselves.

  RaiseError           => 1,
  PrintError           => 0,
  PrintWarn            => 0,
  AutoCommit           => 1,
  TraceLevel           => 0,
  mysql_enable_utf8    => 1,
  mysql_auto_reconnect => 0,
  label                => undef,
  cnfdir               => '.',
  cnf                  => undef,
  cnfgroup             => undef,
  driver               => 'mysql',
  host                 => undef,
  port                 => undef,
  user                 => undef,
  password             => undef,
  schema               => undef

These are the parameters that can be set as class defaults, connector defaults,
connection-specific ('one of'), or a combination of those.  The following subset
are those parameters that govern behaviour of a db connection:

  RaiseError
  PrintError
  PrintWarn
  AutoCommit
  TraceLevel
  mysql_enable_utf8
  mysql_auto_reconnect

L<DBI/"ATTRIBUTES COMMON TO ALL HANDLES"> is the authority on the first five,
their effects and when/why to use them, while L<DBD::mysql/"DATABASE-HANDLES">
is the authority on the latter two, their effects and when/why to use them.

B<Note> that unless C<RaiseError> is changed, connections will throw exceptions
rather than need their return values checked.  This is the most robust style
since it is never fooled by genuinely C<0> or C<undef> return values, and the
exception can be handled at whatever call-level you deem appropriate.  (This
style works best if you use a try-catch mechanism such as C<eval {} or do {}> in
core perl or C<try/catch> in L<Try::Tiny> or L<Error>.)

If your data uses characters that are non-ascii and non-UTF8 then be sure to
override C<mysql_enable_utf8>:

  mysql_enable_utf8 => 0

The following subset are those parameters that govern making a db connection
(DSN) tuple:

  driver
  host
  port
  user
  password
  schema

=over

=item *

It is unlikely you will want C<driver> to be anything other than C<mysql>, the
default.

=item *

Common values to use for C<host> are C<localhost> or C<127.0.0.1>.

=item *

The C<port> param should be set if connecting to a port other than C<3306>.

=item *

The C<schema> param can be set to avoid needing to C<USE someschema>.

=back

None of C<host>, C<port>, C<user>, C<password> is required if you are using
credentials files (recommended).  One of the primary motivations for this
class is to avoid having credentials mixed in with your code.

The following subset is simply to identify a credentials set that will be
passed to the L<DBI> (actually L<DBD::mysql>) with the connection string.

  cnfdir
  cnf
  cnfgroup

=over

=item *

The C<cnfdir> param is the directory containing credentials files.  For
convenience (eg during testing) it defaults to C<.> but best practice is to put
your credentials in a dedicated directory, taking care of access privileges to
keep them secure.

=item *

The C<cnf> param is the credentials file to use, optionally excluding the
C<.cnf> suffix.  The connection builder prepends C<cnfdir> + C</> to this to
generate a fully-qualified filename (as long as C<cnfdir> is a non-empty
string).  However, if C<cnf> identifies a readable file by itself then C<cnfdir>
is ignored.

=item *

The C<cnfgroup> param is the configuration group to read in addition to
C<client>.  eg If you set C<cnfgroup =E<gt> 'myapp'> then connections will use
both of the C<[client]> and C<[myapp]> groups.  Unless set, only the C<[client]>
group is used.

=back

B<Note> that currently the credentials file is passed to L<DBI> without being
parsed first.  (A future version may support parsing parameters from the file.)

Which leaves the following.

  label

=over

=item *

The C<label> param simply holds a string that lets you categorise or tag
connectors.  The motivation for this is when connection pooling you may want to
sub-pool the available connections, but it can also be helpful for debugging.

=back

So in B<summary> the parameters most likely to need overriding are

  cnfdir
  cnf
  schema
  AutoCommit

=head1 DEFAULTS

When you create a connection, its parameters are taken from the attributes of
the class or object creating it, overlaid with any given arguments.  When
designing your code or debugging problems it can be useful to be think of the
hierarchy of attributes.

=over 4

=item 0

Class definition file (F<.pm>)

=item 1

C<use> attributes

=item 2

connector attributes

=item 3

connection-specific parameters

=back

Each level overlays its definitions over those of the level above so that the
attribute value in effect for a connection is the value in the latest level at
the time the connection is created.  Some projects will use only class
attributes, some will use only connector attributes, some will use a
combination, and some will use neither.

=head3 Class definition

The values in the class definition file are shown above (L</"Class/object
parameters">) and will not be changed without a change of major version number.

=head3 Use-time parameters

The calling code can override/set defaults upon C<use>.

  #!/usr/bin/env perl
  use Mojar::Mysql::Connector (
    cnf => '/srv/myapp/cnf/myuser_localhost'
  );

The passed attributes are stored in a class variable and so are shared across
that instance of the perl interpreter.

=head3 Connectors

A connector is created via C<new>

  my $connector = Mojar::Mysql::Connector->new(cnf => 'rw_datawarehouse');

or via the C<-connector> helper

  use Mojar::Mysql::Connector(-connector => 1);
  ...
  my $connector = $self->connector(cnf => 'rw_datawarehouse');

or implicitly via the C<-dbh> helper

  use Mojar::Mysql::Connector(-dbh => 1);
  ...
  my $connector = $self->connector(cnf => 'rw_datawarehouse');

The use of connectors is entirely optional, but their advantages include the
following.

=over 4

=item *

Improve the readability of your code by separating the handle configuration from
handle usage; you can set your parameters in your one-time initialisation and
keep the usage of database handles free of that clutter.

=item *

Having easy access to fresh handles may encourage developers to play safe by
discarding stale ones; don't worry whether earlier code has changed the
C<sql_mode> on the handle, treat yourself to a brand new one.

=item *

Database resources may be better utilised if dormant handles are closed rather
than being kept open as long as possible.  (On the flip-side, if you are making
thousands of new connections per second then your usage needs a rethink.)

=item *

Relying on auto_reconnect can be problematic and difficult to debug.

=item *

There are no objects or references within a connector, so it can be
serialised/deserialised safely and (unlike database handles) can be shared among
threads/processes/invocations safely.

=back

  use Mojar::Mysql::Connector;
  my $connector_source = Mojar::Mysql::Connector->new(
    cnfdir => '/srv/myapp/cfg',
    cnf => 'myuser_sourcehost',
    schema => 'Orders');
  my $connector_target = $connector_source->new(
    cnf => 'myuser_targethost',
    schema => 'Reports');

  while (...) {
    my $dbh_s = $connector_source->connect;
    my $dbh_t = $connector_target->connect;
    ...
    $dbh_s->disconnect;
    $dbh_t->disconnect;
  }

If on the other hand your code makes several connections to one db server and
then all its connections are to a second db server, you can use just one
connector and modify its params when appropriate.

  my $connector = Mojar::Mysql::Connector->new(cnf => 'myuser_source1');
  my $dbh = $connector->connect(schema => 'Preorders';
  ...
  $dbh->disconnect;
  $connector->cnf('myuser_source2');
  $dbh = $connector->connect;
  ...

You could of course achieve the same effect in that example using no connectors
and overriding in the connect.

  my $dbh = Mojar::Mysql::Connector->connect(
    cnf => 'myuser_source1',
    schema => 'Preorders');
  ...
  $dbh->disconnect;
  $dbh = Mojar::Mysql::Connector->connect(
    cnf => 'myuser_source2',
    schema => 'Profiles');
  ...

=head3 DBH connection-specific parameters

The illustrations above have all shown persisting changes to the defaults at
either the class or object level; the new defaults are in effect till the end of
the package/script or till they are changed.  You may also want to pass
parameters that you do not want to persist, a common example being turning off
autocommit.

  $dbh = Mojar::Mysql::Connector->connect(AutoCommit => 0);

or

  $dbh = $connector->connect(AutoCommit => 0);

C<AutoCommit> is C<0> for this connection, but defaults are unaffected.

=head3 Inheritance vs parameter overlay

The above illustrates a general principle of the interplay of inheritance and
parameter overlay; when an object is created it inherits its base values from
the entity it is created from and overlays on top of those any additional
parameters it is passed.  Exactly as you would expect, these additional
parameters have no effect on the entity from which the object was created.

  $connector = Mojar::Mysql::Connector->new(label => 'readonly');
  $dbh = $connector->new(label => 'readwrite');
  say Mojar::Mysql::Connector->label;  # still undef
  say $connector->label;  # still 'readonly'

=head1 CLASS METHODS

=head2 C<new>

  Mojar::Mysql::Connector->new(label => 'cache', cnf => 'myuser_localhost');

Constructor for a connector based on class defaults.  Takes a (possibly empty)
parameter hash.  Returns a connector (Mojar::Mysql::Connector object) the
defaults of which are those of the class overlaid with those passed to the
constructor.

=head2 C<Defaults>

  print Data::Dumper::Dumper(Mojar::Mysql::Connector->Defaults);

Provides access to the defaults hashref that holds the class defaults in order
to help debugging.

=head2 C<connect>

 $dbh1 = Mojar::Mysql::Connector->connect(
   'DBI:mysql:test;host=localhost', 'admin', 's3cr3t', {});
 $dbh2 = Mojar::Mysql::Connector->connect(AutoCommit => 0);
 $dbh3 = Mojar::Mysql::Connector->connect;

Constructor for a connection (db handle).  If the first element passed has
prefix C<DBI:> then it is a DSN string (the traditional route) and so is passed
straight to C<DBI::connect> (L<DBI/"DBI Class Methods">).  Otherwise a DSN is
first constructed.  (The DSN tuple does not persist and is constructed fresh on
each call to C<connect>.)

=head2 C<dsn>

  @dbi_args = Mojar::Mysql::Connector->dsn(
    cnf => 'myuser_localhost', schema => 'test');

A convenience method used internally by connect.  Takes a (possibly empty)
parameter hash.  Returns a four-element array to pass to C<DBI->connect>,
constructed from the default values of the constructing class overlaid with any
additional parameters passed.  Probably the only reason for you using this
method is if you need to use L<DBI> directly but want to avoid the inconvenience
of constructing the parameters yourself.

  use DBI;
  use Mojar::Mysql::Connector (
    cnfdir => '/srv/myapp/cfg',
    cnf => 'myuser_localhost'
  );
  my $dbh = DBI->connect(
    Mojar::Mysql::Connector->dsn(schema => 'foo', AutoCommit => 0));

=head2 C<dsn_as_string>

  Carp::carp(Mojar::Mysql::Connector->dsn_as_string(@dsn));

A convenience method used internally to chop up the four-element array
(particularly the fourth element, the hash ref) into something more readable, eg
for error reporting and debugging.

=head1 OBJECT METHODS

=head2 C<new>

  $connector->new(label => 'transaction', AutoCommit => 0);

Constructor for a connector based on an existing connector's defaults.  Takes a
(possibly empty) parameter hash.  Returns a connector (Mojar::Mysql::Connector
object) the defaults of which are those of the given connector overlaid with
those passed to the constructor.

=head2 C<connect>

  $dbh = $connector->connect(
    'DBI:mysql:test;host=localhost', 'admin', 's3cr3t', {});
  $dbh = $connector->connect(AutoCommit => 0);
  $dbh = $connector->connect;

Constructor for a connection (db handle).  If the first element passed has
prefix C<DBI:> then it is a DSN string (the traditional route) and so is passed
straight to C<DBI::connect> (L<DBI/"DBI Class Methods">).  Otherwise a DSN is
first constructed.  (The DSN tuple does not persist and is constructed fresh on
each call to C<connect>.)

=head2 Attributes

All connector parameters are implemented as attributes with exactly the same
spelling.  So for example you can

  $connector->RaiseError(undef);  # disable RaiseError
  $connector->mysql_enable_utf8(1);  # enable mysql_enable_utf8

These also function as class attributes, but as previously mentioned, safest
to use only as getters rather than class setters.

=head1 DATABASE HANDLE METHODS

=head2 C<mysqld_version>

  if ($dbh->mysqld_version =~ /^5.0/) {...}

Returns the version of the db server connected to; the version part of

  mysqld --version

=head2 C<thread_id>

  $tmp_table_name = q{ConcurrencySafe_}. $dbh->thread_id;

Utility method to get the connection's thread identifier (unique on that db
server at that point in time).

=head2 C<current_schema>

  $schema_name = $dbh->current_schema;

The same string as given by

  SELECT DATABASE();

=head2 C<session_var>

  my ($old) = $dbh->session_var(sql_mode => 'ANSI_QUOTES');
  ...
  $dbh->session_var(sql_mode => $old);

Getter/setter for session variables.  To get a value, simply pass the variable's
name.

  $value = $dbh->session_var('date_format');

In list context returns the old value and the new value; in scalar context
returns the handle to facilitate chaining.

  $dbh->session_var(var1 => ...)
      ->session_var(var2 => ...);

=head2 C<disable_quotes>

  my @ddl = $dbh->disable_quotes->selectrow_array(q{SHOW CREATE ...});

Disable optional quotes around identifiers.  Currently only affects output of
C<SHOW CREATE TABLE>.  If you have unsafe identifiers (eg spaces or keywords)
then those will still be quoted.  Lasts the lifetime of the connection.

=head2 C<enable_quotes>

The inverse of C<disable_quotes>.

=head2 C<disable_fk_checks>

  $dbh->disable_fk_checks->do(q{DROP TABLE ...});

Disable foreign key checks.  Lasts the lifetime of the connection.

=head2 C<enable_fk_checks>

The inverse of C<disable_fk_checks>.

=head2 C<schemata>

  for my $schema (@{$dbh->schemata}) {...}

Returns a hashref of schema names, similar to

  SHOW DATABASES

but does not get fooled by C<lost+found>.

=head2 C<tables_and_views>

  foreach my $table ($dbh->tables_and_views) {...}

Returns a hashref of table and view names, similar to

  SHOW TABLES

See also L<DBI/tables>.

=head2 C<real_tables>

  for my $table (@{$dbh->real_tables}) {...}

Returns a hashref of real table names, similar to

  SHOW TABLES

but excluding views.

=head2 C<views>

  for my $view (@{$dbh->views}) {...}

Returns a hashref of view names, similar to

  SHOW TABLES

but excluding real tables.

=head1 SUPPORT

=head2 Homepage

L<http://niczero.github.com/mojar-mysql>

=head2 Wiki

L<http://github.com/niczero/mojar/wiki>

=head1 RATIONALE

This class was first used in production in 2002.  Before then, connecting to
databases was ugly and annoying.  Setting C<RaiseError> upon every connect was
clumsy and irritating.  In development teams it was tricky checking that all
code was using sensible parameters and awkward ensuring use of risky parameters
(eg C<disable_fk_checks>) was kept local.  As use of this class spread, it had
to be useful in persistent high performance applications as well as many small
scripts and the occasional commandline.  More recently I discovered the Joy of
Mojolicious and employed L<Mojo::Base> to remove unwanted complexity and
eliminate a long-standing bug.  The ensuing fun motivated an extensive rewrite,
fixing broken documentation, improved the tests (thank you travis), and we have,
finally, its public release.  As noted below there are now quite a few smart
alternatives out there but I'm still surprised how little support there is for
keeping passwords out of your codebase and helping you manage multiple
connections.

=head1 SEE ALSO

L<Coro::Mysql>, L<AnyEvent::DBI>, L<DBIx::Custom>, L<DBIx::Connector>, L<DBI>.

=head1 COPYRIGHT AND LICENCE

Copyright (C) 2002--2013, Nic Sandfield.

This program is free software, you can redistribute it and/or modify it under
the terms of the Artistic License version 2.0.

=cut
