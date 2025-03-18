use v5.36;
use utf8;

use Kelp::Test;
use Test::More;
use Test::Deep;
use HTTP::Request::Common;
use KelpBench;

# use mock to avoid the need for DB modules and actual running DB
# (however, we do not test for DB code correctness this way)
package DBMock {
	use v5.36;
	use utf8;

	use Kelp::Base;

	sub random_number ($self, $id)
	{
		return {
			id => $id,
			randomNumber => int(rand(10_000) + 1),
		};
	}

	sub fortune ($self)
	{
		return [
			{
				id => 1,
				message => 'フレームワークのベンチマーク',
			},
			{
				id => 2,
				message => '<script>test</script>',
			},
			{
				id => 3,
				message => '&&/\\+?',
			},
		];
	}

	sub update ($self, $id, $random_number)
	{
		return;
	}
};

my $app = KelpBench->new(mode => 'test', database => DBMock->new);
my $t = Kelp::Test->new(app => $app);
my $world = { randomNumber => re(qr{^\d+$}), id => re(qr{^\d+$}) };

subtest plaintext => sub {
	my $uri = '/plaintext';

	$t->request(GET $uri)
		->content_type_is('text/plain')
		->content_is('Hello, World!');
};

subtest 'json' => sub {
	my $uri = '/json';

	$t->request(GET $uri)
		->json_cmp({ message => 'Hello, World!' });
};

subtest db => sub {
	my $uri = '/db';

	$t->request(GET $uri)
		->json_cmp($world);
};

subtest queries => sub {
	my $uri = '/queries';

	$t->request(GET $uri)
		->json_cmp([$world]);
	$t->request(GET "$uri?queries=3")
		->json_cmp([$world, $world, $world]);
	$t->request(GET "$uri?queries=0")
		->json_cmp([$world]);
};

subtest update => sub {
	my $uri = '/updates';

	$t->request(GET $uri)
		->json_cmp([$world]);
	$t->request(GET "$uri?queries=3")
		->json_cmp([ $world, $world, $world ]);
};

subtest fortunes => sub {
	my $uri = '/fortunes';

	$t->request(GET $uri)
		->content_type_is('text/html')
		->content_like(qr{&lt;script&gt;})
		->content_like(qr{フレームワークのベンチマーク})
		->content_like(qr{Additional fortune added at request time.});
};

done_testing;

