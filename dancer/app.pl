#!/usr/bin/env perl
use strict;
use warnings;

use Dancer ':syntax';
use Dancer::Plugin::Database;
set serializer => 'JSON';

#my $dbh = database({ driver => 'mysql', database => 'test' });
my $dbh = database({ driver => 'mysql', host => 'ip-10-34-150-134.eu-west-1.compute.internal', database => 'test', username => 'root' });

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
