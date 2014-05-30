use strict;
use v5.16;
use utf8;
use JSON::XS qw(encode_json);
use DBI;

my $dbh = DBI->connect_cached(
    'dbi:mysql:database=hello_world;host=localhost;port=3306', 
    'benchmarkdbuser',
    'benchmarkdbpass',
    { AutoInactiveDestroy => 1, mysql_enable_utf8 => 1 }
) || die $!;

my $query = 'SELECT id, randomNumber FROM World WHERE id = ?';
my $header = [qw(Content-Type application/json)];

my $app = sub {
    my $env = shift;
    if ( $env->{PATH_INFO} eq '/json' ) {
        return [ 200, $header, [ encode_json({ message => 'Hello, World!' }) ]];
    }
    elsif ( $env->{PATH_INFO} eq '/db' ) {
        my ($n) = ($env->{QUERY_STRING} || "" ) =~ m!queries=(\d+)!;
        $n //= 1;
        my @rs = map {{id=>$_->[0]+0,randomNumber=>$_->[1]+0}} 
            map { $dbh->selectrow_arrayref($query,{},int rand 10000 + 1) } 1..$n;
        return [ 200, $header, [ encode_json( @rs > 1 ? \@rs : $rs[0] ) ]];
    }
    [ 404, [], ['not found']];
};

$app;
