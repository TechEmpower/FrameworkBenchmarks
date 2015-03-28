use Mojolicious::Lite;
use Mojo::Pg;

use JSON::XS 'encode_json';
use Scalar::Util 'looks_like_number';

# configuration

plugin Config => {
  file => 'app.conf',
  default => {
    database_host => 'localhost',
    hypnotoad => {
      graceful_timeout => 1,
      workers => 8,
    },
    hypnotoad_merge => {},
  },
};

{
  my $merge = app->config->{hypnotoad_merge};
  @{app->config->{hypnotoad}}{keys %$merge} = values %$merge;
}


helper pg => sub { state $pg = Mojo::Pg->new('postgresql://benchmarkdbuser:benchmarkdbpass@' . shift->config->{database_host} . '/hello_world') };

helper render_json => sub { shift->render( data => encode_json(shift), format => 'json' ) }; 

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

get '/plaintext' => sub { shift->render( text => 'Hello, World!' ) };

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
