use Mojolicious::Lite;
use Mojo::Pg;

use Cpanel::JSON::XS 'encode_json';
use Scalar::Util 'looks_like_number';

# configuration

{
  my $nproc = `nproc`;
  app->config(hypnotoad => {
    accepts => 0,
    clients => int( 256 / $nproc ) + 1,
    graceful_timeout => 1,
    requests => 10000,
    workers => $nproc,
  });
}

{
  my $db_host = $ENV{DBHOST} || 'localhost';
  helper pg => sub { state $pg = Mojo::Pg->new('postgresql://benchmarkdbuser:benchmarkdbpass@' . $db_host . '/hello_world') };
}

helper render_json => sub {
  my $c = shift;
  $c->res->headers->content_type('application/json');
  $c->render( data => encode_json(shift) );
};

# Routes

get '/json' => sub { shift->helpers->render_json({message => 'Hello, World!'}) };

get '/db' => sub { shift->helpers->render_query(1, {single => 1}) };

get '/queries' => sub {
  my $c = shift;
  $c->helpers->render_query(scalar $c->param('queries'));
};

get '/fortunes' => sub {
  my $c = shift;
  my $docs = $c->helpers->pg->db->query('SELECT id, message FROM Fortune')->arrays;
  push @$docs, [0, 'Additional fortune added at request time.'];
  $c->render( fortunes => docs => $docs->sort(sub{ $a->[1] cmp $b->[1] }) );
};

get '/updates' => sub {
  my $c = shift;
  $c->helpers->render_query(scalar $c->param('queries'), {update => 1});
};

get '/plaintext' => sub {
  my $c = shift;
  $c->res->headers->content_type('text/plain');
  $c->render( text => 'Hello, World!' );
};

# Additional helpers (shared code)

helper 'render_query' => sub {
  my ($self, $q, $args) = @_;
  $args ||= {};
  my $update = $args->{update};

  $q = 1 unless looks_like_number($q);
  $q = 1   if $q < 1;
  $q = 500 if $q > 500;

  my $r  = [];
  my $tx = $self->tx;

  my $db = $self->helpers->pg->db;

  foreach (1 .. $q) {
    my $id = int rand 10_000;
    my $randomNumber = $db->query('SELECT randomnumber FROM World WHERE id=?', $id)->array->[0];
    $db->query('UPDATE World SET randomnumber=? WHERE id=?', ($randomNumber = 1 + int rand 10_000), $id) if $update;
    push @$r, { id => $id, randomNumber => $randomNumber };
  }

  $r = $r->[0] if $args->{single};
  $self->helpers->render_json($r);
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
