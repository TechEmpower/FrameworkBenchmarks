#!/usr/bin/env perl
use Kelp::Less;
use DBI;

attr dbh => sub {
    my $database = 'hello_world';
    my $host     = 'localhost';
    my $dsn      = 'dbi:mysql:database=hello_world;host=localhost;port=3306';
    DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
};

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/db' => sub {
    my $self = shift;
    my $queries = param('queries') || 1;
    my @response;
    my $sth = $self->dbh->prepare( 'SELECT randomnumber FROM world WHERE id = ?' );
    for ( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        my $res = $sth->execute( $id );
        if ( my $row = $sth->fetchrow_arrayref ) {
            push @response, { id => $id, randomNumber => $row->[0] };
        }
    }
    { \@response }
};

run;
