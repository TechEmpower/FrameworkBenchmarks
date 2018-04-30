<?php

require __DIR__ . '/vendor/autoload.php';

use Amp\Http\Server\Request;
use Amp\Http\Server\RequestHandler;
use Amp\Http\Server\Response;
use Amp\Http\Server\Router;
use Amp\Http\Server\Options;
use Amp\Http\Server\Server;
use Amp\Cluster\Cluster;
use Amp\Promise;
use Amp\Success;
use Psr\Log\NullLogger;

Amp\Loop::run(function () {
    $sockets = yield [
        Cluster::listen("0.0.0.0:8080"),
        Cluster::listen("[::]:8080"),
    ];

    $router = new Router;
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

    $router->addRoute("GET", "/plaintext", new class implements RequestHandler {
        public function handleRequest(Request $request): Promise {
            return new Success(new Response(200, [
                "content-type" => "text/plain",
                "server" => "amphp/http-server",
            ], "Hello, World!"));
        }
    });

    $options = (new Options)->withoutCompression();
    $server = new Server($sockets, $router, new NullLogger, $options);

    yield $server->start();

    Amp\Loop::onSignal(SIGINT, function (string $watcherId) use ($server) {
        Amp\Loop::cancel($watcherId);

        yield $server->stop();
    });
});
