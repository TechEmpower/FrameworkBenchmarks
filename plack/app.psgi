# starman --workers N app.psgi
use v5.16;
use Plack::Builder;
use Plack::Request;
use JSON::XS 'encode_json';
use DBI;

my $dbh = DBI->connect('dbi:mysql:dbname=test', 'root') || die $!;
my $sth = $dbh->prepare('SELECT randomnumber FROM world WHERE id = ?');
my $header = ['Content-Type' => 'application/json'];

builder {
    mount '/json' => sub { [ 200, $header, [ encode_json({ message => 'Hello, World!' })] ] },
    mount '/dbi' => sub { [ 200, $header, [ encode_json([
        map { id => $_->[0] + 0, randomnumber => $_->[1] },
        grep exists $_->[1],
        map [$_, $sth->execute($_) && $sth->fetchrow_array],
        map int rand 10000 + 1,
        1..Plack::Request->new(shift)->param('queries')//1
    ]) ] ] }
};
