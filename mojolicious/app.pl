#!/usr/bin/env perl
use Mojolicious::Lite;
use JSON::XS;
use Memoize;
use DBI;

my $dsn = "dbi:mysql:database=hello_world;host=localhost;port=3306";
my $dbh = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
my $sth = $dbh->prepare("SELECT * FROM World where id = ?");


get '/json' => sub {
    my $self = shift;    
    $self->render( text => JSON::XS::encode_json( { message => 'Hello, world!' } ), format => 'json' );
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
                $self->render( json => { id => $id, randomNumber => $row->{randomNumber} } );
            }
            else {
                push @response,
                  { id => $id, randomNumber => $row->{randomNumber} };
            }
        }
    }
    if ( $queries > 1 ) {
        $self->render( json => \@response );
    }
};

app->start;
