use strict; use feature 'state';
use JSON::XS 'encode_json';
use AnyEvent; use EV;
use AnyEvent::DBI;
use Unix::Processors;
use List::Util qw'min max';

my @dsn = ('dbi:mysql:database=hello_world;host=tfb-database;port=3306', 'benchmarkdbuser', 'benchmarkdbpass');
my $query = 'select randomNumber, id from World where id = ?';

sub {
    my $env = shift;
    my $path = $env->{PATH_INFO};
    return [200, [qw(Content-Type application/json)], [encode_json(+{ message => 'Hello, World!' })]] if $path eq '/json';
    return [200, [qw(Content-Type text/plain)], ['Hello, World!']] if $path eq '/plaintext';
    if ($path eq '/db') {
        state $cpus = Unix::Processors->new->max_online;
        state $dbh = [map AnyEvent::DBI->new(@dsn, on_error => sub { warn }), 1 .. $cpus * 4];
	state $dbh_idx = 0;
    	my ($n) = ($env->{QUERY_STRING} // '' ) =~ m/queries=(\d+)/;
        $n = max(1, min($n//1, 500));
	return sub {
	    my $res = shift;
            my @rs; my $cv = AE::cv;
	    my $done_cb = sub { $res->([200, [qw(Content-Type application/json)], [encode_json($env->{QUERY_STRING} ? \@rs : $rs[0] // {})]]) };
	    for my $qn (1..$n) {
	         $cv->begin($done_cb);
	 	 my $id = int(rand 10000) + 1;
                 $dbh->[$dbh_idx++]->exec($query, $id, sub {
		     my (undef, $rows) = @_;
		     push @rs, map +{ id => $id, randomNumber => 0+ $_->[0] }, @$rows;
		     $cv->end;
                 });
		 $dbh_idx = $dbh_idx % @$dbh if $dbh_idx >= @$dbh;
	    }
	}
    }
    [404, [qw(Content-Type application/json)], ['not found']]
}
