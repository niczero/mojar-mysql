package Mojar::Mysql::Util;
use Mojo::Base -strict;

require Carp;

sub import {
  my $caller = caller;
  my %want = map {$_ => 1} @_;

  no strict 'refs';
  *{"${caller}::find_monotonic_first"} = \&find_monotonic_first
    if $want{find_monotonic_first};

  # Need a closure for lookup => cannot use Exporter
  *{"${caller}::lookup"} = sub { lookup($caller, @_) } if $want{lookup};
}

my $FindRangeSize = 200;

sub find_monotonic_first {
  my ($dbh, $schema, $table, $column, $condition) = @_;
  Carp::croak $column .'Missing required condition' unless defined $condition;
  my $debug = $ENV{MOJAR_MYSQL_UTIL_DEBUG};

  my ($min, $max) = $dbh->selectrow_array(sprintf
q{SELECT MIN(%s), MAX(%s)
FROM %s.%s},
    $column, $column,
    $schema, $table
  );

  if (ref $condition eq 'CODE') {
    # Perl callback
    my $sth_row = $dbh->prepare(sprintf
q{SELECT *
FROM %s.%s
WHERE %s = ?},
      $schema, $table,
      $column
    );

    my $row;
    do {
      # Check minimum
      $row = $dbh->selectrow_hashref($sth_row, undef, $min);
      return $min if $condition->($row);

      # Check maximum
      $row = $dbh->selectrow_hashref($sth_row, undef, $max);
      return undef unless $condition->($row);  # Problem with data

      # Check range
      if ($max - $min <= $FindRangeSize) {
        my $candidate = $min;
        do {
          ($candidate) = $dbh->selectrow_array(sprintf(
q{SELECT MIN(%s)
FROM %s.%s
WHERE ? < %s},
              $column,
              $schema, $table,
              $column),
            undef,
            $candidate
          );
          $row = $dbh->selectrow_hashref($sth_row, undef, $candidate);
        } until $condition->($row);
        return $candidate;
      }

      # Calculate new
      # First find mean
      my $new = $min + int( ($max - $min) / 2 + 0.1 );
      # then find first record after that...
      my ($candidate) = $dbh->selectrow_array(sprintf(
q{SELECT MIN(%s)
FROM %s.%s
WHERE ? <= %s},
          $column,
          $schema, $table,
          $column),
        undef,
        $new
      );
      if ($candidate >= $max) {
        # ...or before that
        ($candidate) = $dbh->selectrow_array(sprintf(
q{SELECT MAX(%s)
FROM %s.%s
WHERE %s <= ?},
            $column,
            $schema, $table,
            $column),
          undef,
          $new
        );
        return undef if $candidate <= $min;  # Problem with data
      }
      $new = $candidate;
      # $min < $candidate < $max

      $row = $dbh->selectrow_hashref($sth_row, undef, $new);
      if ($condition->($row)) {
        $max = $new;
      }
      else {
        $min = $new;
      }
      warn $min, ' : ', $max if $debug;
    } while 1;
  }

  else {
    # SQL where-clause
    my $sth_row = $dbh->prepare(sprintf
q{SELECT COUNT(*)
FROM %s.%s
WHERE %s = ?
AND (%s)},
      $schema, $table,
      $column,
      $condition
    );
    my $sth_range = $dbh->prepare(sprintf
q{SELECT MIN(%s)
FROM %s.%s
WHERE ? <= %s
  AND %s <= ?
  AND (%s)},
      $column,
      $schema, $table,
      $column,
      $column,
      $condition
    );

    # Brute force (for demos)
#    return $dbh->selectrow_arrayref($sth_range, undef, $min, $max)->[0];

    my $satisfied;
    do {
      # Check range
      if ($max - $min <= $FindRangeSize) {
        my ($solution) = $dbh->selectrow_array($sth_range, undef, $min, $max);
        return $solution;
      }

      # Check minimum
      ($satisfied) = $dbh->selectrow_array($sth_row, undef, $min);
      return $min if $satisfied;

      # Check maximum
      ($satisfied) = $dbh->selectrow_array($sth_row, undef, $max);
      return undef unless $satisfied;  # Problem with data

      # Calculate new
      # First find mean
      my $new = $min + int( ($max - $min) / 2 + 0.1 );
      # then find first record after that...
      my ($candidate) = $dbh->selectrow_array(sprintf(
q{SELECT MIN(%s)
FROM %s.%s
WHERE ? <= %s},
          $column,
          $schema, $table,
          $column),
        undef,
        $new
      );
      if ($candidate >= $max) {
        # ...or before that
        ($candidate) = $dbh->selectrow_array(sprintf(
q{SELECT MAX(%s)
FROM %s.%s
WHERE %s <= ?},
            $column,
            $schema, $table,
            $column),
          undef,
          $new
        );
        return undef if $candidate <= $min;  # Problem with data
      }
      $new = $candidate;
      # $min < $candidate < $max

      ($satisfied) = $dbh->selectrow_array($sth_row, undef, $new);
      if ($satisfied) {
        $max = $new;
      }
      else {
        $min = $new;
      }
      warn $min, ' : ', $max if $debug;
    } while 1;
  }
}

sub lookup {
  my ($class, $name, $schema, $table, $key_col, $value_col) = @_;
  Carp::croak 'Wrong number of args' unless @_ == 6;
  Carp::croak qq{Lookup '$name' invalid} unless $name =~ /^[a-zA-Z_]\w*$/;

  my $code = <<EOT;
package $class;
sub $name {
  my \$dbh = shift;
  if (\@_ == 1) {
    return \$dbh->selectrow_arrayref(
q{SELECT $value_col
FROM ${schema}.$table
WHERE $key_col = ?},
      undef,
      \$_[0]
    )->[0];
  }
  \$dbh->do(
q{REPLACE INTO ${schema}.$table
SET $value_col = ?
WHERE $key_col = ?},
    undef,
    \$_[1], \$_[0]
  );
  return;
}
EOT
  warn "-- Lookup $name in $class\n$code\n\n" if $ENV{MOJAR_MYSQL_UTIL_DEBUG};
  Carp::croak "Mojar::Mysql::Util error: $@" unless eval "$code;1";
}

1;
__END__
