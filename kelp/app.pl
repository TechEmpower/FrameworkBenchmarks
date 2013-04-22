#!/usr/bin/env perl
use Kelp::Less;
use DBI;

attr dbh => sub {
    my $database = 'hello_world';
    my $host     = 'localhost';
    my $dsn      = 'dbi:mysql:database=$database;host=$host;port=3306';
    DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
};

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/db' => sub {
    my $self = shift;
    my $queries = param->{queries} || 1;
    my @response;
    for( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        if ( my $row = $self->dbh->quick_select( 'world', { id => $id } ) ) {
            push @response, { id => $id, randomNumber => $row->{randomnumber} };
        }
    }
    { \@response }
};

run;
