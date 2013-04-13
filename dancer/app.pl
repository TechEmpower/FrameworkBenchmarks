#!/usr/bin/env perl
use strict;
use warnings;

use Dancer ':syntax';
use Dancer::Plugin::Database;
set serializer => 'JSON';

my $dbh = database({ driver => 'mysql', database => 'test' });

get '/json' => sub {
    { message => 'Hello, World!' }
};

get '/db' => sub {
    my $queries = params->{queries} || 1;
    my @response;
    for( 1 .. $queries ) {
        my $id = int rand 10000 + 1;
        if ( my $row = $dbh->quick_select( 'world', { id => $id } ) ) {
            push @response, { id => $id, randomNumber => $row->{randomnumber} };
        }
    }
    { \@response }
};

Dancer->dance;
