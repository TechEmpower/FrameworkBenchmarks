use v5.36;
use Mojolicious::Lite;
use Mojo::Pg;
use Mojo::Promise;

use Scalar::Util 'looks_like_number';

# configuration

use constant MAX_DB_CONCURRENCY => 50;

{
  my $nproc = `nproc`;
  app->config(hypnotoad => {
    accepts => 100000,
    clients => MAX_DB_CONCURRENCY,
    graceful_timeout => 1,
    requests => 10000,
    workers => $nproc,
    backlog => 256
  });
}

# Routes

get '/json' => sub ($c) {
  $c->render(json => {message => 'Hello, World!'});
};

get '/db' => sub ($c) {
  $c->helpers->render_query(1, {single => 1});
};

get '/queries' => sub ($c) {
  $c->helpers->render_query(scalar $c->param('queries'));
};

get '/fortunes' => sub ($c) {
  $c->render_later;

  $c->helpers->pg->db->query_p('SELECT id, message FROM Fortune')
    ->then(sub ($query) {
      my $docs = $query->arrays;
      push @$docs, [0, 'Additional fortune added at request time.'];

      $c->render(fortunes => docs => $docs->sort(sub { $a->[1] cmp $b->[1] }));
    });
};

get '/updates' => sub ($c) {
  $c->helpers->render_query(scalar $c->param('queries'), {update => 1});
};

get '/plaintext' => { text => 'Hello, World!', format => 'txt' };

# Additional helpers (shared code)

helper pg => sub {
  state $pg = Mojo::Pg
    ->new('postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world')
    ->max_connections(MAX_DB_CONCURRENCY + 1);
};

helper 'render_query' => sub ($self, $q, $args = {}) {
  $self->render_later;

  $q = 1 unless looks_like_number($q);
  $q = 1   if $q < 1;
  $q = 500 if $q > 500;

  Mojo::Promise->map({concurrency => MAX_DB_CONCURRENCY}, sub {
    my $db = $self->helpers->pg->db;
    my $id = 1 + int rand 10_000;

    my $query = $db->query('SELECT id, randomnumber FROM World WHERE id=?', $id);
    my $number = $query->array->[1];

    if ($args->{update}) {
      $number = 1 + int rand 10_000;
      $db->query('UPDATE World SET randomnumber=? WHERE id=?', $number, $id);
    }

    return Mojo::Promise->resolve([$id, $number]);
  }, 1 .. $q)
    ->then(sub (@responses) {
      my @results;

      foreach my $resp (@responses) {
          push @results, { id => $resp->[0][0], randomNumber => $resp->[0][1] };
      }

      if ($args->{single}) {
        $self->render(json => $results[0]);
      }
      else {
        $self->render(json => \@results);
      }
    });
};

app->start;

__DATA__

@@ fortunes.html.ep
<!DOCTYPE html>
<html>
  <head><title>Fortunes</title></head>
  <body>
    <table>
      <tr><th>id</th><th>message</th></tr>
      % foreach my $doc (@$docs) {
        <tr>
          <td><%= $doc->[0] %></td>
          <td><%= $doc->[1] %></td>
        </tr>
      % }
    </table>
  </body>
</html>

