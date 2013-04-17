# starman --workers N myapp.psgi
use v5.16;
use Plack::Builder;
use Plack::Request;
use Plack::Response;
use JSON::XS 'encode_json';
use DBI;

my $dbh = DBI->connect('dbi:mysql:dbname=test', 'root') || die $!;
my $sth = $dbh->prepare('SELECT id, randomnumber FROM world WHERE id = ?');
my $header = HTTP::Headers->new('Content-Type' => 'application/json');

builder {
    mount '/json' => sub { Plack::Response->new(
        200, $header, encode_json({ message => 'Hello, World!' })
    )->finalize },
    mount '/dbi' => sub {
        my $r = Plack::Request->new(shift);
        $r->new_response( 200, $header, encode_json([ 
            map { id => $_->[0], randomnumber => $_->[1] }, 
            grep exists $_->[1], 
            map [$_, $sth->execute($_) && $sth->fetchrow_array], 
            map int rand 10000 + 1,
            1..$r->param('queries')//1 
        ]) )->finalize
    }
};
