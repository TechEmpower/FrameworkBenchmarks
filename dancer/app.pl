#!/usr/bin/env perl
use strict;
use warnings;

use Dancer ':syntax';
use Dancer::Plugin::Database;
set serializer => 'JSON';

#my $dbh = database({ driver => 'mysql', database => 'test' });
my $dbh = database({ driver => 'mysql', host => 'localhost', database => 'hello_world', username => 'benchmarkdbuser', password => 'benchmarkdbpass' });

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/db' => sub {
    my $queries = params->{queries} || 1;
    my @response;
    for( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        if ( my $row = $dbh->quick_select( 'world', { id => $id } ) ) {
            push @response, { id => $id, randomNumber => $row->{randomNumber} };
        }
    }
    { \@response }
};

Dancer->dance;
