<?php

require __DIR__ . '/vendor/autoload.php';

use Amp\Cluster\Cluster;
use Amp\Coroutine;
use Amp\Http\Server\Request;
use Amp\Http\Server\RequestHandler;
use Amp\Http\Server\Response;
use Amp\Http\Server\Router;
use Amp\Http\Server\Options;
use Amp\Http\Server\Server;
use Amp\Promise;
use Amp\Success;
use Monolog\Logger;

define('DB_HOST', gethostbyname('tfb-database'));
define('CONCURRENCY_LIMIT', 102400);

Amp\Loop::run(function () {
    $sockets = yield [
        Cluster::listen('0.0.0.0:8080',
            (new Amp\Socket\BindContext)->withBacklog(CONCURRENCY_LIMIT)
                                        ->withReusePort()
                                        ->withTcpNoDelay()
        )
    ];

    $router = new Router;

    $config = Amp\Mysql\ConnectionConfig::fromString(
        'host='.DB_HOST.' user=benchmarkdbuser password=benchmarkdbpass db=hello_world'
    );

    $connector = new Amp\Mysql\CancellableConnector(
        new class implements Amp\Socket\Connector {
            public function connect(
                string $uri,
                ?Amp\Socket\ConnectContext $context = null,
                ?Amp\CancellationToken $token = null
            ): Amp\Promise {
                $context = $context ?? new Amp\Socket\ConnectContext;
                $context = $context->withTcpNoDelay();

                return Amp\Socket\connector()->connect($uri, $context, $token);
            }
        }
    );

    $mysql = new Amp\Mysql\Pool($config, 512 * 50, 300, $connector);

    // Case 1 - JSON
    $router->addRoute('GET', '/json', new class implements RequestHandler {
        public function handleRequest(Request $request): Promise {
            return new Success(new Response(200, [
                'Content-Type' => 'application/json',
                'Server' => 'amphp/http-server',
            ], \json_encode([
                'message' => 'Hello, World!',
            ])));
        }
    });

    // Case 2 - Single Query
    $router->addRoute('GET', '/db', new class ($mysql) implements RequestHandler {
        private $mysql;

        public function __construct($mysql) {
            $this->mysql = $mysql;
        }

        public function handleRequest(Request $request): Promise {
            return new Coroutine($this->doHandleRequest($request));
        }

        private function doHandleRequest($request) {
            $statement = yield $this->mysql->prepare('SELECT * FROM World WHERE id = ?');
            $result = yield $statement->execute([mt_rand(1, 10000)]);

            if (yield $result->advance()) {
                $item = $result->getCurrent();
            } else {
                $item = null;
            }

            return new Response(200, [
                'Content-Type' => 'application/json',
                'Server' => 'amphp/http-server',
            ], \json_encode($item));
        }
    });

    // Case 3 - Multiple Queries
    $router->addRoute('GET', '/queries', new class ($mysql) implements RequestHandler {
        private $mysql;

        public function __construct($mysql) {
            $this->mysql = $mysql;
        }

        public function handleRequest(Request $request): Promise {
            return new Coroutine($this->doHandleRequest($request));
        }

        private function doHandleRequest($request) {
            $query = $request->getUri()->getQuery();
            \parse_str($query, $queryParams);

            $queries = (int) ($queryParams['q'] ?? 1);
            if ($queries < 1) {
                $queries = 1;
            } elseif ($queries > 500) {
                $queries = 500;
            }

            $items = [];

            $statement = yield $this->mysql->prepare('SELECT * FROM World WHERE id = ?');

            while ($queries--) {
                $items[] = new Coroutine($this->execute($statement));
            }

            return new Response(200, [
                'Content-Type' => 'application/json',
                'Server' => 'amphp/http-server',
            ], \json_encode(yield $items));
        }

        private function execute($statement) {
            $result = yield $statement->execute([mt_rand(1, 10000)]);
            yield $result->advance();

            return $result->getCurrent();
        }
    });

    // Case 4 - Fortunes
    $router->addRoute('GET', '/fortunes', new class ($mysql) implements RequestHandler {
        private $mysql;

        public function __construct($mysql) {
            $this->mysql = $mysql;
        }

        public function handleRequest(Request $request): Promise {
            return new Coroutine($this->doHandleRequest($request));
        }

        private function doHandleRequest($request) {
            $result = yield $this->mysql->query('SELECT * FROM Fortune');
            $items = [];

            while (yield $result->advance()) {
                $item = $result->getCurrent();
                $items[$item['id']] = $item['message'];
            }

            $items[0] = 'Additional fortune added at request time.';

            \asort($items);

            \ob_start();

            require __DIR__ . '/fortunes.php';

            return new Response(200, [
                'Content-Type' => 'text/html; charset=utf-8',
                'Server' => 'amphp/http-server',
            ], \ob_get_clean());
        }

    });

    // Case 6 - Plaintext
    $router->addRoute('GET', '/plaintext', new class implements RequestHandler {
        public function handleRequest(Request $request): Promise {
            return new Success(new Response(200, [
                'Content-Type' => 'text/plain',
                'Server' => 'amphp/http-server',
            ], 'Hello, World!'));
        }
    });

    $logger = new Logger('Worker-' . Cluster::getId());
    $logger->pushHandler(Cluster::createLogHandler());

    $logger->info('Using ' . get_class(Amp\Loop::get()));

    $options = (new Options)
        ->withoutCompression()
        ->withConnectionLimit(CONCURRENCY_LIMIT)
        ->withConnectionsPerIpLimit(CONCURRENCY_LIMIT);

    $server = new Server($sockets, $router, $logger, $options);

    yield $server->start();

    Amp\Loop::onSignal(\SIGINT, function (string $watcherId) use ($server) {
        Amp\Loop::cancel($watcherId);

        yield $server->stop();
    });
});
