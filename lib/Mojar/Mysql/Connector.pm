# ============
package Mojar::Mysql::Connector;
# ============
use DBI 1.4.3 qw(:sql_types);
use Mojo::Base 'DBI';
# Register subclass structure
__PACKAGE__->init_rootclass;

our $VERSION = 2.032;

use File::Spec::Functions 'catfile';

sub import {
  my ($pkg, %param) = @_;
  my $caller = caller;
  # Helpers
  $param{-connector} ||= 1 if exists $param{-dbh} && $param{-dbh};
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
  if (%param and keys %{$pkg->Defaults}) {
    # Already have defaults => die
    die "Redefining class defaults for $pkg";
  }
  @{ $pkg->Defaults }{keys %param} = values %param if %param;
  # Debugging
  $pkg->trace($param{TraceLevel})
    if exists $param{TraceLevel} && defined $param{TraceLevel};
}

# ------------
# Class attributes
# ------------

has Defaults => sub { {} };

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

# ------------
# Private functions
# ------------

sub croak { require Carp; goto &Carp::croak; }

# ------------
# Public methods
# ------------

sub new {
  my ($proto, %param) = @_;
  # $proto may contain defaults to be cloned
  # %param may contain defaults for overriding
  my %defaults = ref $proto ? ( %{ ref($proto)->Defaults }, %$proto ) : %{ $proto->Defaults };
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
          . ($param{schema} // '')
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

# ============
package Mojar::Mysql::Connector::db;
# ============
@Mojar::Mysql::Connector::db::ISA = 'DBI::db';

use Scalar::Util 'looks_like_number';

# ------------
# Private functions
# ------------

sub croak { require Carp; goto &Carp::croak; }

# ------------
# Public methods
# ------------

sub mysqld_version { shift->get_info(18) }
# 18 : SQL_DBMS_VER

sub thread_id { shift->{mysql_thread_id} || 0 }

sub current_schema {
  my ($self) = @_;
  my $schema_name;
  eval {
    ($schema_name) = $self->selectrow_array(
q{SELECT DATABASE()});
    1
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
      1
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
    1
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
  my @schemata;
  eval {
    my $sql = q{SHOW DATABASES};
    $sql .= sprintf " LIKE '%s'", $args[0] if defined $args[0];
    @schemata = $self->selectcol_arrayref($sql, $args[1])
      or die;
    @schemata = grep !/^lost\+found/, @schemata;
    1
  }
  or do {
    my $e = $@ // '';
    croak "Failed to list schemata\n$e";
  };
  return \@schemata;
}

sub tables_and_views {
  my ($self, @args) = @_;
  # args[0] : schema
  # args[1] : table pattern
  # args[2] : type
  # args[3] : attr
  $args[2] //= 'TABLE,VIEW';
  my @tables;
  eval {
    my $sth = $self->table_info('', @args);
    @tables = map $_->[2], @{$sth->fetchall_arrayref};
    1
  }
  or do {
    my $e = $@ // '';
    croak "Failed to list tables\n$e";
  };
  return \@tables;
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

1
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
  my $read_dbh = $read_connector->connect(
    auto_reconnect => 1
  );
  my $write_dbh = $write_connector->connect;

Employing a helper.

  use Mojar::Mysql::Connector (
    cnfdir => '/var/local/auth/myapp',
    cnf => 'rw_localhost',
    schema => 'Users',
    -dbh => 1
  );
  sub do_some_db_doodah {
    my $self = shift;
    my $dbh = $self->dbh;
    ...
  }

From the commandline.

  perl -MMojar::Mysql::Connector="cnf,ro_localhost,schema,Users,-dbh,1"
    -E'say join qq{\n}, @{main->dbh->real_tables}'

=head1 DESCRIPTION

MySQL-specific extension (subclass) to DBI in order to improve convenience,
security, and error handling.  Supports easy use of credential/init files, akin
to

  mysql --defaults-file=$CRED_FILE

It aims to reduce boilerplate, verbosity, mistakes, and parameter overload, but
above all it tries to make it quick and easy to Do The Right Thing.

As the name implies, the class provides connector objects -- containers for
storing and updating your connection parameters.  When you call C<connect>, the
connector returns a handle created using its retained parameters plus any
call-time parameters you may have used in the call.  You don't however have to
use connectors and for simple usage it can be easier to use C<connect> directly
from the class.

=head1 USAGE

One of the main advantages of using this subclass in place of the standard
L<DBI> is the provision of a set of defaults that can be overridden.  For
instance, a script can define a C<cnfdir> and C<cnf> early on, then redefine
C<cnf> for connections to a different server.  Various examples of usage are
given below (L</"Using defaults">) after the parameters themselves have been
introduced.

=head2 Class/object parameters

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
(DSN) series:

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
(sub)class is to avoid having credentials mixed in with your code.

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

  AutoCommit
  cnfdir
  cnf
  schema

=head2 Using defaults

As you would expect, there is a hierarchy for setting parameters that take
effect for new connections.

=over

=item 0.

Class definition file (F<.pm>)

=item 1.

'use' params

=item 2.

connector params

=item 3.

dbh (connection-specific) params

=back

Each level overlays its definitions over those of the level above so that the
value in effect for a connection is the value in the latest level at the time
the connection is created.  This flexibility might be a bit confusing at first,
but examples below should show that individual usage is very simple.  It is
expected that some users will use only class defaults, some will use only object
defaults, some will use a combination, and some will use neither.

=head3 Class definition

The values in the class definition file are shown above (L</"Class/object
parameters">) and should stay static through subsequent released versions.

=head3 Use-time parameters

The calling code can override/set defaults upon C<use>.

  #!/usr/bin/env perl
  use Mojar::Mysql::Connector (
    cnfdir => '/srv/myapp/cfg',
    cnf    => 'myuser_localhost',
    schema => 'Stats'
  );

That sets the class defaults for the remainder of that script or package, and
for simple cases that is the only place your code needs to deal with connection
parameters.  When using a persistent multi-application environment (mod_perl,
plack, hypnotoad, ...) bear in mind that class parameters are shared across the
process (perl interpreter instance).

=head3 Connectors

Connectors are objects that you pack with your preferred connection parameters
and then have ready to supply you with database connections.  Their use is
entirely optional, but their advantages include the following.

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

=head2 Class methods

=head3 C<new>

  Mojar::Mysql::Connector->new(label => 'cache', cnf => 'myuser_localhost');

Constructor for a connector based on class defaults.  Takes a (possibly empty)
parameter hash.  Returns a connector (Mojar::Mysql::Connector object) the
defaults of which are those of the class overlaid with those passed to the
constructor.

=head3 C<Defaults>

  print Data::Dumper::Dumper(Mojar::Mysql::Connector->Defaults);

Provides access to the defaults hashref that holds the class defaults in order
to help debugging.  Each default has a getter/setter of the same name, but as
described previously, it's fairly risky to change class defaults during runtime
if you have other code sharing those defaults.

=head3 C<connect>

 $dbh1 = Mojar::Mysql::Connector->connect(
   'DBI:mysql:test;host=localhost', 'admin', 's3cr3t', {});
 $dbh2 = Mojar::Mysql::Connector->connect(AutoCommit => 0);
 $dbh3 = Mojar::Mysql::Connector->connect;

Constructor for a connection (db handle).  If the first element passed has
prefix C<DBI:> then it is a DSN string (the traditional route) and so is passed
straight to C<DBI::connect> (L<DBI/"DBI Class Methods">).  Otherwise a DSN is
first constructed.  (The DSN series does not persist and is constructed fresh on
each call to C<connect>.)

=head3 C<dsn>

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

=head3 C<dsn_as_string>

  Carp::carp(Mojar::Mysql::Connector->dsn_as_string(@dsn));

A convenience method used internally to chop up the four-element array
(particularly the fourth element, the hash ref) into something more readable, eg
for error reporting and debugging.

=head2 Object methods

=head3 C<new>

  $connector->new(label => 'transaction', AutoCommit => 0);

Constructor for a connector based on an existing connector's defaults.  Takes a
(possibly empty) parameter hash.  Returns a connector (Mojar::Mysql::Connector
object) the defaults of which are those of the given connector overlaid with
those passed to the constructor.

=head3 C<connect>

  $dbh = $connector->connect(
    'DBI:mysql:test;host=localhost', 'admin', 's3cr3t', {});
  $dbh = $connector->connect(AutoCommit => 0);
  $dbh = $connector->connect;

Constructor for a connection (db handle).  If the first element passed has
prefix C<DBI:> then it is a DSN string (the traditional route) and so is passed
straight to C<DBI::connect> (L<DBI/"DBI Class Methods">).  Otherwise a DSN is
first constructed.  (The DSN series does not persist and is constructed fresh on
each call to C<connect>.)

=head2 Attributes

All connector parameters are implemented as attributes with exactly the same
spelling.  So for example you can

  $connector->RaiseError(undef);  # disable RaiseError
  $connector->mysql_enable_utf8(1);  # enable mysql_enable_utf8

These also function as class attributes, but as previously mentioned, safest
to use only as getters rather than class setters.

=head2 Database handle methods

=head3 C<mysqld_version>

  if ($dbh->mysqld_version =~ /^5.0/) {...}

Returns the version of the db server connected to; the version part of

  mysqld --version

=head3 C<thread_id>

  $tmp_table_name = q{ConcurrencySafe_}. $dbh->thread_id;

Utility method to get the connection's thread identifier (unique on that db
server at that point in time).

=head3 C<current_schema>

  $schema_name = $dbh->current_schema;

The same string as given by

  SELECT DATABASE();

=head3 C<session_var>

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

=head3 C<disable_quotes>

  my @ddl = $dbh->disable_quotes->selectrow_array(q{SHOW CREATE ...});

Disable optional quotes around identifiers.  Currently only affects output of
C<SHOW CREATE TABLE>.  If you have unsafe identifiers (eg spaces or keywords)
then those will still be quoted.  Lasts the lifetime of the connection.

=head3 C<enable_quotes>

The inverse of C<disable_quotes>.

=head3 C<disable_fk_checks>

  $dbh->disable_fk_checks->do(q{DROP TABLE ...});

Disable foreign key checks.  Lasts the lifetime of the connection.

=head3 C<enable_fk_checks>

The inverse of C<disable_fk_checks>.

=head3 C<schemata>

  for my $schema (@{$dbh->schemata}) {...}

Returns a hashref of schema names, similar to

  SHOW DATABASES

but does not get fooled by C<lost+found>.

=head3 C<tables_and_views>

  foreach my $table ($dbh->tables_and_views) {...}

Returns a hashref of table and view names, similar to

  SHOW TABLES

See also L<DBI/tables>.

=head3 C<real_tables>

  for my $table (@{$dbh->real_tables}) {...}

Returns a hashref of real table names, similar to

  SHOW TABLES

but excluding views.

=head3 C<views>

  for my $view (@{$dbh->views}) {...}

Returns a hashref of view names, similar to

  SHOW TABLES

but excluding real tables.

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

L<Coro::Mysql>, L<AnyEvent::DBI>, L<DBIx::Custom>, L<DBIx::Connector>.

=cut
