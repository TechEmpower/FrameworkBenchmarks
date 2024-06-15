<?php

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface as Request;
use React\EventLoop\Loop;
use React\MySQL\ConnectionInterface as DbConnection;
use React\MySQL\Factory as DbFactory;
use React\Http\Message\Response;
use React\MySQL\QueryResult;
use React\Promise\PromiseInterface;

use function React\Promise\all;

/** @return Closure(Request):ResponseInterface */
function requestHandler(): Closure
{
    $connection = establishDbConnection('benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world?idle=0.5');

    $world = static function (int $id) use ($connection): PromiseInterface {
        return $connection->query('SELECT id,randomNumber FROM World WHERE id=?', [$id]);
    };

    $fortune = static function () use ($connection): PromiseInterface {
        return $connection->query('SELECT id,message FROM Fortune');
    };

    $update = static function (int $id, int $randomNumber) use ($connection): PromiseInterface {
        return $connection->query('UPDATE World SET randomNumber=? WHERE id=?', [$randomNumber, $id]);
    };

    return static function (Request $request) use ($world, $fortune, $update): ResponseInterface | PromiseInterface {
        return match($request->getUri()->getPath()) {
            '/plaintext' => Response::plaintext('Hello, World!'),
            '/json'      => Response::json(['message' => 'Hello, World!']),
            '/db'        => db($world),
            '/fortunes'  => fortune($fortune),
            '/query'     => query(queryCount($request), $world),
            '/update'    => updateraw(queryCount($request), $world, $update),
            // '/info'      => info(),
            default      => new Response(404, [], 'Error 404'),
        };
    };
}

function establishDbConnection(
    #[SensitiveParameter]
    string $uri,
): DbConnection {
    $connection = (new DbFactory())->createLazyConnection($uri);

    $interrupt = $connection->quit(...);

    $connection->on('close', static function () use (&$interrupt) {
        Loop::removeSignal(SIGINT, $interrupt);
        Loop::removeSignal(SIGTERM, $interrupt);
    });

    Loop::addSignal(SIGINT, $interrupt);
    Loop::addSignal(SIGTERM, $interrupt);

    return $connection;
}

/** @param Closure(int):PromiseInterface $world */
function db(Closure $world): PromiseInterface
{
    $id = mt_rand(1, 10000);

    return $world($id)->then(
        static fn (QueryResult $result): ResponseInterface => Response::json($result->resultRows[0]),
    );
}

function queryCount(Request $request): int
{
    $count = (int) ($request->getQueryParams()['q'] ?? 1);

    if ($count > 1) {
        return min($count, 500);
    }

    return 1;
}

/** @param Closure(int):PromiseInterface $world */
function query(int $queryCount, Closure $world): PromiseInterface
{
    $processQueries = static function (int $count) use ($world): iterable {
        while ($count--) {
            $id = mt_rand(1, 10000);

            yield $world($id)->then(static fn (QueryResult $result): array => $result->resultRows[0]);
        }
    };

    return all($processQueries($queryCount))
        ->then(static fn (array $result): ResponseInterface => Response::json($result));
}

/**
 * @param Closure(int):PromiseInterface $world
 * @param Closure(int, int):PromiseInterface $update
 */
function updateraw(int $queryCount, Closure $world, Closure $update): PromiseInterface
{
    $processQueries = static function (int $count) use ($world, $update): iterable {
        while ($count--) {
            $id = mt_rand(1, 10000);

            yield $world($id)->then(
                static function (QueryResult $result) use ($update): PromiseInterface {
                    $updated = $result->resultRows[0];
                    $updated['randomNumber'] = mt_rand(1, 10000);

                    return $update($updated['id'], $updated['randomNumber'])
                        ->then(static fn (): array => $updated);
                }
            );
        }
    };

    return all($processQueries($queryCount))
        ->then(static fn (array $result): ResponseInterface => Response::json($result));
}

function fortune(Closure $fortune): PromiseInterface
{
    $formatResult = static function (array $rows): string {
        $rows[] = ['id' => 0, 'message' => 'Additional fortune added at request time.'];
        usort($rows, static fn (array $one, array $other) => $one['message'] <=> $other['message']);

        $html = '';

        foreach ($rows as $row) {
            $message = htmlspecialchars($row['message'], ENT_QUOTES, 'UTF-8');

            $html .= "<tr><td>{$row['id']}</td><td>{$message}</td></tr>";
        }

        return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
    };

    return $fortune()->then(
        static fn (QueryResult $result): ResponseInterface => Response::html($formatResult($result->resultRows)),
    );
}

/* function info()
{
    ob_start();
    phpinfo();
    return new Response(200, ['Content-Type' => 'text/plain'], ob_get_clean());
}
 */
