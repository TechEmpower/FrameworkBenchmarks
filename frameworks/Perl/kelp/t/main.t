use strict;
use warnings;
use utf8;

use Kelp::Test;
use Test::More;
use Test::Deep;
use HTTP::Request::Common;

my $t = Kelp::Test->new( psgi => 'app.pl');
my $world = { randomNumber => re(qr{^\d+$}), id => re(qr{^\d+$}) };

subtest 'json' => sub {
    $t->request( GET '/json' )->json_cmp( { message => 'Hello, World!' } );
};

subtest plaintext => sub {
    $t->request( GET '/plaintext' )
      ->content_type_is('text/plain')
      ->content_is('Hello, World!');
};

subtest db => sub {
    for my $uri (qw{/db /db/mongo}) {
        $t->request( GET $uri )->json_cmp($world);
    }
};

subtest queries => sub {
    for my $uri (qw{/queries /queries/mongo}) {
        $t->request( GET $uri )->json_cmp($world);
        $t->request( GET "$uri?queries=3" )
          ->json_cmp( [ $world, $world, $world ] );
        $t->request( GET "$uri?queries=0" )->json_cmp($world);
    }
};

subtest update => sub {
    for my $uri (qw{/update /update/mongo}) {
        $t->request( GET $uri )->json_cmp([$world]);
        $t->request( GET "$uri?queries=3" )
          ->json_cmp( [ $world, $world, $world ] );
    }
};

subtest fortunes => sub {
    for my $uri (qw{/fortunes /fortunes/mongo}) {
        $t->request( GET $uri )
          ->content_type_is('text/html')
          ->content_like(qr{&lt;script&gt;})
          ->content_like(qr{フレームワークのベンチマーク})
          ->content_like(qr{Additional fortune added at request time.});
    }
};

done_testing;
