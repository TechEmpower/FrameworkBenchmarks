#!/usr/bin/env perl

use Kelp::Less;
use HTML::Escape 'escape_html';
use MongoDB;
use DBI;
use utf8;

module 'JSON::XS';

my $mongo   = MongoDB::MongoClient->new( host => 'localhost', port => 27017 );
my $mdb     = $mongo->get_database('hello_world');
my $world   = $mdb->get_collection('World');
my $fortune = $mdb->get_collection('Fortune');

my $dbh = DBI->connect(
    "dbi:mysql:database=hello_world;host=localhost;port=3306",
    'benchmarkdbuser',
    'benchmarkdbpass',
    { RaiseError => 0, PrintError => 0, mysql_enable_utf8 => 1 }
);

my @sth = map { $dbh->prepare($_) } (
    "SELECT * FROM World WHERE id = ?",
    "SELECT * FROM Fortune",
    "UPDATE World SET randomNumber = ? WHERE id = ?",
);

get '/json' => sub {
    { message => 'Hello, World!' };
};

get '/db/?db' => sub {
    my ( $self, $db ) = @_;
    query( $db // 'mongo', 1 );
};

get '/queries/?db' => sub {
    my ( $self, $db ) = @_;
    query( $db // 'mongo', $self->param('queries') );
};

get '/fortunes/?db' => sub {
    my ( $self, $db ) = @_;
    $db //= 'mongo';
    my @objects;
    if ( $db eq 'mongo' ) {
        my $cursor = $fortune->query( {} );
        @objects = $cursor->all;
    }
    else {
        $sth[1]->execute();
        @objects = @{ $sth[1]->fetchall_arrayref( {} ) };
    }
    push @objects, { id => 0, message => "Additional fortune added at request time." };
    fortunes( \@objects );
};

get '/update/?db' => sub {
    my ( $self, $db ) = @_;
    $db //= 'mongo';

    my $arr = query( $db, $self->param('queries') );
    $arr = [$arr] unless ref($arr) eq 'ARRAY';
    for my $row (@$arr) {
        $row->{randomNumber} = int( rand(10_000) ) + 1;
        if ( $db eq 'mongo' ) {
            $world->update( { _id => $row->{id} },
                { randomNumber => $row->{randomNumber} } );
        }
        else {
            $row->{randomNumber} = int( rand(10_000) ) + 1;
            $sth[2]->execute( $row->{randomNumber}, $row->{id} );
        }
    }

    return $arr;
};

get '/plaintext' => sub {
    shift->res->text->render('Hello, World!');
};

run;

sub query {
    my ( $db, $count ) = @_;
    $count //= 1;
    $count = 1 if ( $count !~ /^\d+$/ || $count < 1 );
    $count = 500 if $count > 500;
    my @response;
    for ( 1 .. $count ) {
        my $id = int rand 10000 + 1;
        my $row;
        if ( $db eq 'mongo' ) {
            $row = $world->find_one( { _id => $id } );
        }
        else {
            $sth[0]->execute($id);
            $row = $sth[0]->fetchrow_hashref;
        }
        if ($row) {
            if ( $count == 1 ) {
                return { id => $id, randomNumber => $row->{randomNumber} };
            }
            else {
                push @response,
                  { id => $id, randomNumber => $row->{randomNumber} };
            }
        }
    }
    return \@response;
}

sub fortunes {
    my ($objects) = @_;
    my $res = q[<!DOCTYPE html><html><head><title>Fortunes</title></head>];
    $res .= q[<body><table><tr><th>id</th><th>message</th></tr>];

    for my $item ( sort { $a->{message} cmp $b->{message} } @$objects ) {
        my $id = $item->{id};
        my $message = escape_html( $item->{message} );

        # HTML::Escape encodes apostrophe as &#39; because IE8 does not
        # support &apos;. We forse an &apos; here in order to pass the
        # test
        $message =~ s/&#39/&apos/g;
        $res .= "<tr><td>$id</td><td>$message</td></tr>";
    }

    $res .= q[</table></body></html>];
    return $res;
}
