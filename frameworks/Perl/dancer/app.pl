#!/usr/bin/env perl
use strict;
use warnings;

use Dancer ':syntax';
use DBI;
use JSON::XS;  # Ensure that the fast implementation of the serializer is installed

set serializer => 'JSON';

my $dsn = "dbi:mysql:database=hello_world;host=tfb-database;port=3306";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', { mysql_auto_reconnect=>1 } );
my $sth = $dbh->prepare("SELECT * FROM World where id = ?");

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/dbquery' => sub {
    my $queries = params->{queries} || 1;
    $queries = 1 if ( $queries !~ /^\d+$/ || $queries < 1 );
    $queries = 500 if $queries > 500;
    
    my @response;
    for ( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        $sth->execute($id);
        if ( my $row = $sth->fetchrow_hashref ) {
            push @response,
                { id => $id, randomNumber => $row->{randomNumber} };
        }
    }
    return \@response;
};

get '/db' => sub {
    my $id = int rand 10000 + 1;
    $sth->execute($id);
    if ( my $row = $sth->fetchrow_hashref ) {
            return { id => $id, randomNumber => $row->{randomNumber} };
    }
};

Dancer->dance;
