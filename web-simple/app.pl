#!/usr/bin/env perl
use Web::Simple;
use JSON::XS;
use DBI;

my $dsn = "dbi:mysql:database=hello_world;host=localhost";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
my $sth = $dbh->prepare('SELECT * FROM World where id = ?');

sub dispatch_request {
 sub (/json) {
    [ 200, [ 'Content-type' => 'application/json; charset=utf-8', ],
      [ encode_json({ message => 'Hello, World!' }) ] ];
  },
  sub (/db + ?queries~) {
    my ($self, $queries) = @_;
    my @response;
    for ( 1 .. $queries || 1 ) {
        my $id = int rand 10000 + 1;
        $sth->execute($id);
        if ( my @row = $sth->fetchrow_array ) {
          push @response, { id => $id, randomNumber => $row[1] };
        }
        $sth->finish();
    }
    [ 200, [ 'Content-type' => 'application/json; charset=utf-8', ],
      [ encode_json(\@response)] ];
  }
}

__PACKAGE__->run_if_script;
