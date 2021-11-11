use Mojolicious::Lite;
use Mojo::Pg;
use Mojo::Promise;

use Cpanel::JSON::XS 'encode_json';
use Scalar::Util 'looks_like_number';
use Data::Dumper;

# configuration

{
  my $nproc = `nproc`;
  app->config(hypnotoad => {
    accepts => 0,
    clients => int( 256 / $nproc ) + 1,
    graceful_timeout => 1,
    requests => 10000,
    workers => $nproc,
    backlog => 256
  });
}

{
  my $db_host = 'tfb-database';
  helper pg => sub { state $pg = Mojo::Pg->new('postgresql://benchmarkdbuser:benchmarkdbpass@' . $db_host . '/hello_world')->max_connections(50) };
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
  $c->render_later;
  my $docs = $c->helpers->pg->db->query_p('SELECT id, message FROM Fortune')
  ->then(sub{
    my $docs = $_[0]->arrays;
    push @$docs, [0, 'Additional fortune added at request time.'];
    $c->render(fortunes => docs => $docs->sort(sub{ $a->[1] cmp $b->[1] }) )
  });
};

get '/updates' => sub {
  my $c = shift;
  $c->helpers->render_query(scalar $c->param('queries'), {update => 1});
};

get '/plaintext' => { text => 'Hello, World!', format => 'txt' };

# Additional helpers (shared code)

helper 'render_query' => sub {
  my ($self, $q, $args) = @_;
  $self->render_later;
  $args ||= {};
  my $update = $args->{update};

  $q = 1 unless looks_like_number($q);
  $q = 1   if $q < 1;
  $q = 500 if $q > 500;

  my $r  = [];
  my $tx = $self->tx;


  my @queries;
  foreach (1 .. $q) {
    my $id = 1 + int rand 10_000;

    push @queries, $self->helpers->pg->db->query_p('SELECT id,randomnumber FROM World WHERE id=?', $id)
    ->then(sub{
	my $randomNumber = $_[0]->array->[0];

	return Mojo::Promise->new->resolve($id, $randomNumber)
        ->then(sub{
	   if($update) {
		$randomNumber = 1 + int rand 10_000;
		return Mojo::Promise->all(
		    Mojo::Promise->new->resolve($_[0], $randomNumber),
		    $self->helpers->pg->db->query_p('UPDATE World SET randomnumber=? WHERE id=?', $randomNumber, $id)
		)
		->then(sub {
	            return $_[0];
	        })
	    }
            return [shift, shift];
        })
    });
  }

  Mojo::Promise->all(@queries)
  ->then(sub{
      my @responses = @_;
      foreach my $resp (@responses) {
          push @$r, { id => $resp->[0][0], randomNumber => $resp->[0][1] };
      }
      $r = $r->[0] if $args->{single};
      $self->helpers->render_json($r);
  })

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
