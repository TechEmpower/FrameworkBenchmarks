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
use Psr\Log\NullLogger;

Amp\Loop::run(function () {
    $sockets = yield [
        Cluster::listen("0.0.0.0:8080"),
        Cluster::listen("[::]:8080"),
    ];

    $router = new Router;

    // Case 1 - JSON
    $router->addRoute("GET", "/json", new class implements RequestHandler {
        public function handleRequest(Request $request): Promise {
            return new Success(new Response(200, [
                "content-type" => "application/json",
                "server" => "amphp/http-server",
            ], \json_encode([
                "message" => "Hello, World!",
            ])));
        }
    });

    // Case 2 - Single Query
    $router->addRoute("GET", "/db", new class implements RequestHandler {
        private $mysql;

        public function __construct() {
            $this->mysql = Amp\Mysql\pool("host=tfb-database user=benchmarkdbuser password=benchmarkdbpass db=hello_world");
        }

        public function handleRequest(Request $request): Promise {
            return new Coroutine($this->doHandleRequest($request));
        }

        private function doHandleRequest($request) {
            $statement = yield $this->mysql->prepare("SELECT * FROM World WHERE id = ?");
            $result = yield $statement->execute([mt_rand(1, 10000)]);

            if (yield $result->advance()) {
                $item = $result->getCurrent();
            } else {
                $item = null;
            }

            return new Response(200, [
                "content-type" => "application/json",
                "server" => "amphp/http-server",
            ], \json_encode($item));
        }
    });

    // Case 3 - Multiple Queries
    $router->addRoute("GET", "/queries", new class implements RequestHandler {
        private $mysql;

        public function __construct() {
            $this->mysql = Amp\Mysql\pool("host=tfb-database user=benchmarkdbuser password=benchmarkdbpass db=hello_world");
        }

        public function handleRequest(Request $request): Promise {
            return new Coroutine($this->doHandleRequest($request));
        }

        private function doHandleRequest($request) {
            $query = $request->getUri()->getQuery();
            \parse_str($query, $queryParams);

            $queries = (int) ($queryParams["queries"] ?? 1);
            if ($queries < 1) {
                $queries = 1;
            } elseif ($queries > 500) {
                $queries = 500;
            }

            $items = [];

            $statement = yield $this->mysql->prepare("SELECT * FROM World WHERE id = ?");

            for ($i = 0; $i < $queries; $i++) {
                $items[] = new Coroutine($this->execute($statement));
            }

            return new Response(200, [
                "content-type" => "application/json",
                "server" => "amphp/http-server",
            ], \json_encode(yield $items));
        }

        private function execute($statement) {
            $result = yield $statement->execute([mt_rand(1, 10000)]);
            yield $result->advance();

            return $result->getCurrent();
        }
    });

    // Case 4 - Fortunes
    $router->addRoute("GET", "/fortunes", new class implements RequestHandler {
        private $mysql;

        public function __construct() {
            $this->mysql = Amp\Mysql\pool("host=tfb-database user=benchmarkdbuser password=benchmarkdbpass db=hello_world");
        }

        public function handleRequest(Request $request): Promise {
            return new Coroutine($this->doHandleRequest($request));
        }

        private function doHandleRequest($request) {
            $result = yield $this->mysql->query("SELECT * FROM Fortune");
            $items = [];

            while (yield $result->advance()) {
                $item = $result->getCurrent();
                $items[$item["id"]] = $item["message"];
            }

            $items[0] = "Additional fortune added at request time.";

            \asort($items);

            \ob_start();

            require __DIR__ . '/fortunes.php';

            return new Response(200, [
                "content-type" => "text/html; charset=utf-8",
                "server" => "amphp/http-server",
            ], \ob_get_clean());
        }

        private function execute($statement) {
            $result = yield $statement->execute([mt_rand(1, 10000)]);
            yield $result->advance();

            return $result->getCurrent();
        }
    });

    // Case 6 - Plaintext
    $router->addRoute("GET", "/plaintext", new class implements RequestHandler {
        public function handleRequest(Request $request): Promise {
            return new Success(new Response(200, [
                "content-type" => "text/plain",
                "server" => "amphp/http-server",
            ], "Hello, World!"));
        }
    });

    $logger = new Monolog\Logger("server");
    $logger->pushHandler(Amp\Cluster\createLogHandler());

    $logger->info("Using " . get_class(Amp\Loop::get()));

    $options = (new Options)
        ->withoutCompression()
        ->withConnectionLimit(16384)
        ->withConnectionsPerIpLimit(16384);

    $server = new Server($sockets, $router, $logger, $options);

    yield $server->start();

    Amp\Loop::onSignal(SIGINT, function (string $watcherId) use ($server) {
        Amp\Loop::cancel($watcherId);

        yield $server->stop();
    });
});
