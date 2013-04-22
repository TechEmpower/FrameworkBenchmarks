#!/usr/bin/env perl
use Kelp::Less;
use DBI;

attr dbh => sub {
    my $database = 'test';
    my $host     = 'ip-10-34-150-134.eu-west-1.compute.internal';
    my $dsn      = 'dbi:mysql:database=$database;host=$host;port=3306';
    DBI->connect( $dsn, 'root', '', {} );
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
