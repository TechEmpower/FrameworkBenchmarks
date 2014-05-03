#!/usr/bin/env perl
use Kelp::Less;
use DBI;

my $dsn = "dbi:mysql:database=hello_world;host=localhost;port=3306";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
my $sth = $dbh->prepare("SELECT * FROM World where id = ?");

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/db' => sub {
    my $self = shift;
    my $queries = $self->param('queries') || 1;
    my @response;
    for ( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        $sth->execute($id);
        if ( my $row = $sth->fetchrow_hashref ) {
            if ( $queries == 1 ) {
                return { id => $id, randomNumber => $row->{randomNumber} };
            }
            else {
                push @response,
                  { id => $id, randomNumber => $row->{randomNumber} };
            }
        }
    }
    return \@response;
};

run;
