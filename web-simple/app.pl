#!/usr/bin/env perl
use Web::Simple;
use JSON::XS;
use DBI;

my $dsn = "dbi:mysql:database=hello_world;host=localhost";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', { RaiseError => 1 });
my $sth = $dbh->prepare('SELECT * FROM World where id = ?');

sub dispatch_request {
 sub (/json) {
    [ 200, [ 'Content-type' => 'application/json', ],
      [ encode_json({ message => 'Hello, World!' }) ] ];
  },
  sub (/db + ?queries~) {
    my ($self, $queries) = @_;
    $queries ||= 1;
    my $rand;
    my @response;
    if ($queries == 1) {
        my $id = int(rand 10000) + 1;
        $sth->execute($id);
        $sth->bind_col(2, \$rand);
        if ( my @row = $sth->fetch ) {
            [ 200, [ 'Content-type' => 'application/json', ], 
              [ encode_json({ id => $id, randomNumber => $rand })] ];
        }
    }
    else {
      for ( 1 .. $queries ) {
          my $id = int(rand 10000) + 1;
          $sth->execute($id);
          $sth->bind_col(2, \$rand);
          if ( my @row = $sth->fetch ) {
              push @response, { id => $id, randomNumber => $rand };
          }
      }
      [ 200, [ 'Content-type' => 'application/json', ], [ encode_json(\@response)] ];
    }
  }
}

__PACKAGE__->run_if_script;
