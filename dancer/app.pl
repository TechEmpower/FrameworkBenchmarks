#!/usr/bin/env perl
use strict;
use warnings;

use Dancer ':syntax';
use DBI;

set serializer => 'JSON';

my $dsn = "dbi:mysql:database=hello_world;host=localhost;port=3306";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
my $sth = $dbh->prepare("SELECT * FROM World where id = ?");

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/db' => sub {
    my $queries = params->{queries} || 1;
    my @response;
    for ( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        $sth->execute($id);
        if ( my $row = $sth->fetchrow_hashref ) {
            push @response,
              { id => $id, randomNumber => $row->{randomnumber} };
        }
    }
    return \@response;
};

Dancer->dance;
