#!/usr/bin/env perl
use Mojolicious::Lite;
use Mojolicious::Plugin::Database;

plugin 'database', {
    dsn      => 'dbi:mysql:dbname=test',
    username => 'root',
    password => ''
};

get '/json' => sub {
    my $self = shift;
    $self->render( json => { message => 'Hello, world!' } );
};

get '/db' => sub {
    my $self = shift;
    my $queries = $self->param('queries') || 1;
    my @response;
    my $sth = $self->db->prepare( 'SELECT randomnumber FROM world WHERE id = ?' );
    for ( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        my $res = $sth->execute( $id );
        if ( my $row = $sth->fetchrow_arrayref ) {
            push @response, { id => $id, randomNumber => $row->[0] };
        }
    }
    $self->render( json => \@response );
};

app->start;
