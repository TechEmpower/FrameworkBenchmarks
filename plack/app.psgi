use v5.16;
use Plack::Builder;
use Plack::Request;
use JSON::XS 'encode_json';
use DBI;

my $dbh = DBI->connect_cached(
    'dbi:mysql:database=hello_world;host=localhost;port=3306', 
    qw(benchmarkdbuser benchmarkdbpass)
) || die $!;

my $sth = $dbh->prepare_cached('SELECT randomNumber FROM World WHERE id = ?');
my $header = [qw(Content-Type application/json)];

builder {
    mount '/json' => sub { [ 200, $header, [ encode_json({ message => 'Hello, World!' })] ] },
    mount '/db' => sub { [ 200, $header, [ encode_json([
        map { id => $_->[0] + 0, randomNumber => $_->[1] },
        grep exists $_->[1],
        map [$_, $sth->execute($_) && $sth->fetchrow_array],
        map int rand 10000 + 1,
        1..Plack::Request->new(shift)->param('queries')//1
    ]) ] ] } };


