#!/usr/bin/env perl
use Mojolicious::Lite;
use DBI;

my $dsn = "dbi:mysql:database=hello_world;host=localhost;port=3306";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
my $sth = $dbh->prepare("SELECT * FROM World where id = ?");

get '/json' => sub {
    my $self = shift;
    $self->render( json => { message => 'Hello, world!' } );
};

get '/db' => sub {
    my $self = shift;
    my $queries = $self->param('queries') || 1;
    my @response;
    for ( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        $sth->execute($id);
        if ( my $row = $sth->fetchrow_hashref ) {
            push @response,
              { id => $id, randomNumber => $row->{randomnumber} };
        }
    }
    $self->render( json => \@response );
};

app->start;
