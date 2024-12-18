use v5.40;
use warnings;
use Feersum::Runner;
use EV; use AnyEvent;
use DBI 'SQL_INTEGER';
use DBD::Pg ':async';
use Scalar::Util 'weaken';
use List::Util qw'min max pairmap';
use JSON::XS;
use Text::Xslate;
use LMDB_File qw':flags :error';

use constant {
    host_port => $ENV{host_port} || '0.0.0.0:8080',
    debug => $ENV{debug} // 0,
    db => lc($ENV{db} || 'postgres'), # postgres / mysql / maria (will use for constant folding)
    db_name => $ENV{db_name} || 'hello_world',
    db_host => $ENV{db_host} || 'tfb-database',
    db_port => $ENV{db_port},
    db_user => $ENV{db_user} || 'benchmarkdbuser',
    db_pass => $ENV{db_pass} || 'benchmarkdbpass',
    empty => [], o => +{},
    reconnect_interval => 60,
    max_db_connections => 512,
    max_update_tries => 3
};
use constant max_batch_update_size => 1; # db eq 'postgres' ? 5 : 10; # rule of thumb
use constant server => qw'Server Feersum';
use constant {
    text => [server, qw'Content-Type text/plain'],
    json => [server, qw'Content-Type application/json'],
    html => [server, 'Content-Type', 'text/html; charset=utf-8'],
    nocontent => [server],
};

my @dsn = (
    (sprintf 'dbi:%s:port=%d;host=%s;database=%s;',
     (db eq 'mysql' ? ('mysql', db_port // 3306) :
      db eq 'maria' ? ('MariaDB', db_port // 3306) :
      db eq 'postgres' ? ('Pg', db_port // 5432)
      : die 'unknown db'), db_host, db_name),
    db_user, db_pass,
    +{qw'AutoCommit 1 RaiseError 0 PrintError 1',
      (db eq 'maria' ? (qw'mariadb_server_prepare 1 mariadb_ssl 0') :
       db eq 'mysql' ? (qw'mysql_server_prepare 1 mysql_ssl 0 mysql_get_server_pubkey 1') :
       db eq 'postgres' ? (qw'pg_server_prepare 1 sslmode 0') : ())}
);

chomp(my $cpus = `nproc`);
say "$cpus cpus available" if debug;
my $pool_size = int max_db_connections / $cpus; # number of db connections in each worker
my $js = JSON::XS->new;

my $html = render();
cache('init');

my %prepare = (
    world => ['select randomNumber, id from World where id = ?', SQL_INTEGER],
    fortune => ['select id, message from Fortune'],
    update1 => ['update World set randomNumber = ? where id = ?', (SQL_INTEGER) x 2],
    (map {
        'update'.$_ => 
            [sprintf(
                (db eq 'mysql' || db eq 'maria') ? 'with t(v,i) as (values %s) update World w join t on t.i = w.id set w.randomNumber = t.v' :
                db eq 'postgres' ? 'with t(v,i) as (values %s) update World w set randomNumber = t.v from t where t.i = w.id' : undef,
                (join ',', ((db eq 'mysql' || db eq 'maria') ? 'row(?,?)' : '(?,?)') x $_)
            ), (SQL_INTEGER) x ($_ * 2)]
    } 2..max_batch_update_size)
);

my ($pool, $cache);
my $w = EV::fork sub { # child init 
    $pool = db_pool($pool_size, \@dsn, \%prepare); # db connection pool in each worker
    $cache = cache('use'); # cache
};

my %route = controllers();

my $runner = Feersum::Runner->new(
    pre_fork => $cpus,
    quiet => !debug, keepalive => 1,
    max_connection_reqs => 1000,
    read_timeout => 60,
    listen => [host_port]
)->run(sub ($h) { ($route{$h->path} // $route{404})->($h) });

sub controllers {(
    '/plaintext', sub ($h) { $h->send_response(200, text, \'Hello, World!') },
    '/json', sub ($h) { $h->send_response(200, json, \$js->encode(+{ message => 'Hello, World!' })) },
    (map +('/db', $_, '/queries', $_, '/updates', $_ ), sub ($h) {
        my ($n) = (my $q = $h->query // '') =~ m/queries=(\d+)/a;
        $n = max(1, min($n//1, 500));
        my ($cv, @rs) = (AE::cv);
        my $on_done = sub { $h->send_response(200, json, \$js->encode($q ? \@rs : ($rs[0] // o))) };
        $cv->begin(
            $h->path ne '/updates'
            ? $on_done # select
            : sub { # update
                if (@rs) {
                    my ($i, $j) = (0, 0);
                    my $cv = AE::cv;
                    $cv->begin($on_done);
                    while () {
                        $j = min($i + max_batch_update_size - 1, $#rs);
                        say "$i $j" if debug;
                        $cv->begin;
                        $_->{randomNumber} = int(rand 10000) + 1 for @rs[$i..$j];
                        my $tries = max_update_tries;
                        my $st = 'update'.($j - $i + 1);
                        my $args = [map @$_{qw/randomNumber id/}, @rs[$i..$j]];
                        my $update = sub ($rv = undef, $sth_or_e = undef) {
                            $cv->end, return if $rv;
                            say 'retryin update on '.$sth_or_e if $tries < max_update_tries;
                            say 'fail to update on '.max_update_tries.' tries ' and $cv->end unless $tries--;
                            db_execute($pool, $st, $args, __SUB__);
                        };
                        $update->();
                        $i += max_batch_update_size;
                        last if $i >= @rs;
                    }
                    $cv->end;
                } else { $on_done->() }
            }
        );
        for (1..$n) {
            my $id = int(rand 10000) + 1;
            $cv->begin;
            db_execute($pool, world => [$id], sub ($rows, $sth) {
                push @rs, @{$sth->fetchall_arrayref(+{ randomNumber => 1, id => 1 })} if $rows > 0;
                $cv->end
            });
        }
        $cv->end
    }),
    '/fortunes' => sub ($h) {
        db_execute($pool, fortune => empty, sub ($rows, $sth) {
            $h->send_response(200, html, \$html->render('fortune.tx', +{ rows => [
                sort { $a->[1] cmp $b->[1] }
                @{$sth->fetchall_arrayref},
                [0, 'Additional fortune added at request time.']
            ]}))
        });
    },
    '/cached-queries' => sub ($h) {
        my ($n) = (my $q = $h->query // '') =~ m/count=(\d+)/a;
        $n = max(1, min($n//1, 500));
        my @rs = map +{ id => $_ , randomNumber => $cache->($_) }, map int(rand 10000) + 1, 1..$n;
        $h->send_response(200, json, \$js->encode(\@rs));
    },
    '/' => sub ($h) { $h->send_response(204, nocontent, empty) },
    404 => sub ($h) { $h->send_response(404, nocontent, empty) }
)}

sub render {
    my $t = Text::Xslate->new(path => +{
        (my $file = 'fortune.tx') => <<~\html =~ s/(?<=[\r\n])\s+//sgr
        <!DOCTYPE html>
        <html>
          <head><title>Fortunes</title></head>
          <body>
            <table>
              <tr><th>id</th><th>message</th></tr>
              : for $rows -> $i {
                <tr><td><: $i.0 :></td><td><: $i.1 :></td></tr>
              : }
            </table>
          </body>
        </html>
        html
    });
    $t->load_file($file);
    $t
}

sub cache ($type = 'init') {
    my $path = '/dev/shm/feersum';
    say "clearing $path" and unlink glob "$path*" if $type eq 'init' && -e $path;
    my $env = LMDB::Env->new($path, +{
        mapsize => 1024*512,
        flags => MDB_WRITEMAP|MDB_NOSYNC|MDB_NOMETASYNC|MDB_NOTLS|MDB_NOSUBDIR|MDB_NORDAHEAD
    }) or die $LMDB_File::last_err;
    if ($type eq 'init') {
        die unless defined(my $tx = $env->BeginTxn);
        my $handle = $tx->open(undef, MDB_CREATE|MDB_INTEGERKEY);
        my $dbh = DBI->connect(@dsn);
        $tx->put($handle, $_->[0], pack S => $_->[1]) for @{$dbh->selectall_arrayref('select id, randomNumber from World')};
        $tx->commit;
        $dbh->disconnect;
        say 'cache populated' if debug;
        return;
    }
    my $tx = $env->BeginTxn(MDB_RDONLY);
    my $handle = $tx->open(undef, MDB_INTEGERKEY);
    sub ($k) { $tx->renew; $tx->get($handle, $k, my $v); unpack S => $v }
}

sub db_pool ($size, $dsn, $prepare = undef) {
    my %pool = (slot => [], active => +{}, free => [], pending => [], prepare => $prepare);
    db_connect(\%pool, $_, $dsn) for 0 .. $size - 1;
    \%pool
}

sub db_connect ($pool, $id, $dsn) {
    say "db[$id] connection.." if debug;
    my $dbh = DBI->connect(@$dsn);
    unless ($dbh) {
        warn sprintf 'err: %s. will try reconnect %d sec', $DBI::errstr, reconnect_interval;
        $pool->{slot}[$id] = AE::timer +reconnect_interval, 0, sub { db_connect($pool, $id, $dsn) }; # try later
        return
    }
    my $fd = db eq 'maria' ? $dbh->mariadb_sockfd : db eq 'mysql' ? $dbh->mysql_fd : db eq 'postgres' ? $dbh->{pg_socket} : undef;
    open my $fh, "<&=", $fd or die $!; # dup handle
    state $st_opt = +{
        db eq 'maria' ? (mariadb_async => 1) :
        db eq 'mysql' ? (async => 1) :
        db eq 'postgres' ? (pg_async => PG_ASYNC + PG_OLDQUERY_CANCEL) : ()
    };
    my %conn = (
        id => $id, db => $dbh, fd => $fd, fh => $fh, dsn => $dsn,
        st => +{ $pool->{prepare} ? (pairmap {
            my $sth = $dbh->prepare($b->[0], $st_opt);
            $sth->bind_param($_, undef, $b->[$_]) for 1..$#$b;
            ($a, $sth)
        } %{$pool->{prepare}}) : () },
        connected => 1,
    );
    $conn{w} = EV::io $fh, EV::READ, sub {
        my $e;
        {   ;
            $e = 'inactive', last unless defined(my $st = $conn{active});
            if ($st) { # executed st
                $e = 'nost', last unless my $sth = $conn{st}{$st};
                $e = 'unready', last unless
                    db eq 'maria' ? $sth->mariadb_async_ready :
                    db eq 'mysql' ? $sth->mysql_async_ready :
                    db eq 'postgres' ? $sth->pg_ready : undef;
                $e = 'noresult', $sth->finish unless defined(
                    my $rows = 
                        db eq 'maria' ? $sth->mariadb_async_result :
                        db eq 'mysql' ? $sth->mysql_async_result :
                        db eq 'postgres' ? $sth->pg_result : undef
                );
                say "db[$id $fd] calling cb: ".$st if debug;
                if (my $cb = $conn{cb}) { $cb->($rows, $e // $sth) }
                else { say "db[$id $fd] no handler for response with $rows rows" }
                $sth->finish unless $e;
            } else { # db do
                $e = 'nodb', last unless my $dbh = $conn{db};
                $e = 'unready', last unless
                    db eq 'maria' ? $dbh->mariadb_async_ready :
                    db eq 'mysql' ? $dbh->mysql_async_ready :
                    db eq 'postgres' ? $dbh->pg_ready : undef;
                $e = 'noresult' unless defined(
                    my $rv =
                        db eq 'maria' ? $dbh->mariadb_async_result :
                        db eq 'mysql' ? $dbh->mysql_async_result :
                        db eq 'postgres' ? $dbh->pg_result : undef
                );
                say "db[$id $fd] calling cb: db do query" if debug;
                if (my $cb = $conn{cb}) { $cb->($rv, $e) }
                else { say "db[$id $fd] no handler response with $rv return" }
            }
            say "db[$id $fd] error: $e " if debug && $e;
            say "db[$id $fd] finish" if debug;
            delete $conn{active};
            delete $pool->{active}{$id};
            push @{$pool->{free}}, \%conn;
            if (defined(my $pending = shift @{$pool->{pending}})) {
                my $code = shift @$pending;
                $code->($pool, splice @$pending)
            }
            return
        }
        say "db[$id $fd] $e" if debug;
        if (eof($fh) || (my $inactive = $e eq 'inactive')) {
            say "db[$id $fd] disconnected" if debug;
            delete @conn{qw/w connected/};
            $conn{db}->disconnect if $inactive;
            $conn{cb}->(-1, undef) if $conn{st} && $conn{active} && $conn{cb};
            db_connect($pool, $id, $dsn); # reconnect
        } else {
            say "db[$id $fd] stalled?";
        }
    };
    say "db[$id $fd] connected" if debug;
    $pool->{slot}[$id] = \%conn;
    weaken(my $weak = $pool->{slot}[$id]);
    push @{$pool->{free}}, $weak;
    if (defined(my $pending = shift @{$pool->{pending}})) {
        my $code = shift @$pending;
        $code->($pool, splice @$pending)
    }
}

sub db_execute ($pool, $st, $args, $cb) {
    say 'db executing..' if debug;
    while (my $conn = shift @{$pool->{free}}) {
        (debug and say 'skip unconnected'), next unless defined($conn) && $conn->{connected};
        say 'on connection..'.$conn->{id} if debug;
        if ($conn->{st}{$st}->execute(@$args)) {
            (@$conn{qw/cb active/}, $pool->{active}{$conn->{id}}) = ($cb, $st, 1);
            return;
        } else {
            say 'error: ', $conn->{st}{$st}->errstr;
            db_connect($pool, @$conn{qw/id dsn/}); # reconnect
            next;
        }
    }
    say '..put to pending..' if debug;
    push @{$pool->{pending}}, [__SUB__, $st, $args, $cb];
}

sub db_do ($pool, $query, $args, $cb) {
    say 'db doing..' if debug;
    state $db_opt = +{
        db eq 'maria' ? (mariadb_async => 1) :
        db eq 'mysql' ? (async => 1) :
        db eq 'postgres' ? (pg_async => PG_ASYNC + PG_OLDQUERY_CANCEL) : ()
    };
    while (my $conn = shift @{$pool->{free}}) {
        (debug and say 'skip unconnected'), next unless defined($conn) && $conn->{connected};
        say 'on connection..'.$conn->{id} if debug;
        if ($conn->{db}->do($query, $db_opt, defined($args) ? @$args : ())) {
            (@$conn{qw/cb active/}, $pool->{active}{$conn->{id}}) = ($cb, 0, 1);
            return;
        } else {
            say 'error: ', $conn->{db}->errstr;
            db_connect($pool, @$conn{qw/id dsn/}); # reconnect
            next;
        }
    }
    say '..put to pending..' if debug;
    push @{$pool->{pending}}, [__SUB__, $query, $args, $cb];
}
