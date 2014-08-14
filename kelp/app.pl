#!/usr/bin/env perl
use Kelp::Less;
use DBI;

my $dsn  = "dbi:mysql:database=hello_world;host=localhost;port=3306";
my $dbh  = DBI->connect( $dsn, 'benchmarkdbuser', 'benchmarkdbpass', {} );
my $sth  = $dbh->prepare("SELECT * FROM World WHERE id = ?");
my $sth1 = $dbh->prepare("SELECT * FROM Fortune");
my $sth2 = $dbh->prepare("UPDATE World SET randomNumber = ? WHERE id = ?");

get '/populate' => sub {
    $dbh->do("DELETE FROM World");
    $dbh->do("DELETE FROM Fortune");
    srand;

    # Add some random numbers
    my @rand = map {'(' . $_ . ',' . int(rand(10_000)) . ')'} (1 .. 10_000);
    $dbh->do(q[INSERT INTO World (id, randomNumber) VALUES ] . join(',', @rand));

    # Add some fortunes
    my @fortunes = map { '("' . 'x' x (int(rand(20)) + 1) . '")' } (1 .. 30);
    $dbh->do(q[INSERT INTO Fortune (message) VALUES ] . join(',', @fortunes));

    "OK";
};

get '/json' => sub {
    my $self = shift;
    { message => 'Hello, World!' };
};

get '/db' => sub {
    query(1);
};

get '/queries' => sub {
    my $self = shift;
    my $count = $self->param('queries') || 1;
    if ($count !~ /^\d+$/) {
        $count = 1;
    }
    query( $count > 500 ? 500 : $count );
};

get '/fortunes' => sub {
    my $self = shift;
    $sth1->execute();
    my $fortunes = $sth1->fetchall_arrayref({});
    $self->template( 'fortunes', { fortunes => $fortunes } );
};

get '/updates' => sub {
    my $self  = shift;
    my $count = $self->param('queries');
    my $arr   = query( $count > 500 ? 500 : $count );
    for my $row (@$arr) {
        $row->{randomNumber} = int( rand(10_000) ) + 1;
        $sth2->execute( $row->{randomNumber}, $row->{id} );
    }

    $arr;
};

get '/plaintext' => sub {
    my $self = shift;
    $self->res->text->render('Hello, World!');
};

run;

sub query {
    my $count = shift;
    my @response;
    for ( 1 .. $count ) {
        my $id = int rand 10000 + 1;
        $sth->execute($id);
        if ( my $row = $sth->fetchrow_hashref ) {
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
