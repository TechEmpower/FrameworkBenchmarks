use Mojolicious::Lite;
use Mango;

use JSON::XS 'encode_json';
use Scalar::Util 'looks_like_number';

# configuration

plugin JSONConfig => {
  file => 'app.conf',
  default => {
    database_host => 'localhost',
    workers => 8,
  },
};

app->config->{hypnotoad}{workers} = app->config->{workers};

# Database connections

helper mango   => sub { state $mango = Mango->new('mongodb://'. shift->config->{database_host} . ':27017') };
helper db      => sub { state $db = shift->mango->db('hello_world') };
helper world   => sub { shift->db->collection('World') };
helper fortune => sub { shift->db->collection('Fortune') };

# JSON::XS renderer

helper render_json => sub { shift->render( data => encode_json(shift), format => 'json' ) }; 

# Routes

get '/json' => sub { shift->render_json({message => 'Hello, World!'}) };

get '/db' => sub { shift->render_query(1) };

get '/queries' => sub {
  my $c = shift;
  $c->render_query($c->param('queries'));
};

get '/fortunes' => sub {
  my $c = shift->render_later;
  $c->fortune->find->all(sub{
    my ($cursor, $err, $docs) = @_;
    push @$docs, { _id => 0, message => 'Additional fortune added at request time.' };
    $c->render( fortunes => docs => $docs );
  });
};

get '/updates' => sub {
  my $c = shift;
  $c->render_query($c->param('queries'), 1);
};

get '/plaintext' => sub { shift->render( text => 'Hello, World!' ) };

# Additional helpers (shared code)

helper 'render_query' => sub {
  my ($self, $q, $update) = @_;
  $self->render_later;

  $q = 1 unless looks_like_number($q);
  $q = 500 if $q > 500;

  my $r  = [];
  my $tx = $self->tx;

  my $delay = Mojo::IOLoop->delay(sub{
    $self->render_json($r) unless $tx->is_finished;
  });

  my $world = $self->world;

  foreach (1 .. $q) {
    my $id = int rand 10_000;
    my $end = $delay->begin;
    $world->find_one({_id => $id} => sub {
      my ($world, $err, $doc) = @_;
      if ($update) { $doc->{randomNumber} = 1 + int rand 10_000 };
      push @$r, { id => $id, randomNumber => $doc->{randomNumber} };
      $update ? $world->save($doc, $end) : $end->();
    });
  }

  # use this line if not running under a Mojolicious server
  # $delay->wait unless $delay->ioloop->is_running;
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
      % foreach my $doc (sort { $a->{message} cmp $b->{message} } @$docs) {
        <tr>
          <td><%= $doc->{_id}     %></td>
          <td><%= $doc->{message} %></td>
        </tr>
      % }
    </table>
  </body>
</html>
