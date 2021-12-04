use feature 'state';
use Cpanel::JSON::XS 'encode_json';
use DBI;
use List::Util qw'min max';

sub {
    state $dbh = DBI->connect(
	'dbi:mysql:database=hello_world;host=tfb-database;port=3306',
	'benchmarkdbuser', 'benchmarkdbpass',
	+{ qw'RaiseError 0 PrintError 0 mysql_enable_utf8 1' }
    ) || die $!;
    state $sth = $dbh->prepare('select id,randomnumber from world where id = ?');
    my $env = shift;
    my $path = $env->{PATH_INFO};
    return [200, [qw(Content-Type application/json)], [encode_json(+{ message => 'Hello, World!' })]] if $path eq '/json';
    return [200, [qw(Content-Type text/plain)], ['Hello, World!']] if $path eq '/plaintext';
    if ( $path eq '/db' ) {
	my ($n) = ($env->{QUERY_STRING} // '' ) =~ m/queries=(\d+)/;
	$n = max(1, min($n//1, 500));
	my @rs = map {
	    $sth->execute(my $id = int(rand 10000) + 1);
	    +{ id => $id, randomNumber => 0+ $sth->fetch->[0] }
	} 1..$n;
	return [ 200, [qw(Content-Type application/json)], [encode_json($env->{QUERY_STRING} ? \@rs : $rs[0] // {})]];
    }
    [ 404, [], ['not found']];
}
