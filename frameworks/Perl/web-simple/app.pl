#!/usr/bin/env perl
use Web::Simple;
use JSON::XS;
use DBI;

sub get_database_handle {
    DBI->connect_cached('dbi:mysql:database=hello_world;host=tfb-database', 'benchmarkdbuser', 'benchmarkdbpass', { RaiseError => 1 });
}

sub dispatch_request {
    sub (/json) {
        [ 200, [ 'Content-type' => 'application/json', ], [ encode_json({ message => 'Hello, World!' }) ] ];
    },
    sub (/db) {
        my ($id, $random_number);
        my $dbh = get_database_handle;
        my $sth = $dbh->prepare_cached('SELECT id, randomNumber FROM World where id = ?');
        $sth->execute(int(rand 10000) + 1);
        $sth->bind_columns(\$id, \$random_number);
        $sth->fetch;
        $sth->finish;
        [ 200, [ 'Content-type' => 'application/json', ], [ encode_json({ id => $id, randomNumber => $random_number })] ];
    },
    sub (/query + ?queries~) {
        my ($self, $queries) = @_;
        $queries //= 1;
        $queries = 1 if ($queries !~ /^\d+$/ || $queries < 1);
        $queries = 500 if $queries > 500;
        if ($queries == 1) {
            my ($id, $random_number);
            my $dbh = get_database_handle;
            my $sth = $dbh->prepare_cached('SELECT id, randomNumber FROM World where id = ?');
            $sth->execute(int(rand 10000) + 1);
            $sth->bind_columns(\$id, \$random_number);
            $sth->fetch;
            $sth->finish;
            [ 200, [ 'Content-type' => 'application/json', ], [ encode_json([{ id => $id, randomNumber => $random_number }])] ];
        }
        else {
            my @worlds;
            for (1 .. $queries) {
                my ($id, $random_number);
                my $dbh = get_database_handle;
                my $sth = $dbh->prepare_cached('SELECT id, randomNumber FROM World where id = ?');
                $sth->execute(int(rand 10000) + 1);
                $sth->bind_columns(\$id, \$random_number);
                $sth->fetch;
                $sth->finish;
                push @worlds, { id => $id, randomNumber => $random_number };
            }
            [ 200, [ 'Content-type' => 'application/json', ], [ encode_json(\@worlds)] ];
        }
    }
}

__PACKAGE__->run_if_script;
