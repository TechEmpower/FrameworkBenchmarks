<?php declare(strict_types=1);

include __DIR__ . '/vendor/autoload.php';

use Ripple\Database\Interface\AbstractResultSet;
use Ripple\Database\MySQL\Client;
use Ripple\Net\Http\Server;
use Ripple\Net\Http\Server\Request;
use Ripple\Sync\Channel;
use Ripple\Sync\WaitGroup;
use Ripple\Worker;
use Ripple\Worker\Manager;
use Ripple\Net\Http;

use function Co\go;
use function Co\wait;

class Setup
{
    /**
     * @var string
     */
    public static string $dateFormatted;

    /**
     * @var null|callable
     */
    private static $fortunesRenderer = null;

    /**
     * @return void
     */
    public static function dateRefresh(): void
    {
        try {
            $date = new DateTime('now', new DateTimeZone('GMT'));
        } catch (Throwable) {
            return;
        }

        Setup::$dateFormatted = $date->format('D, d M Y H:i:s T');
    }

    /**
     * @return int
     */
    public static function randomInt(): int
    {
        try {
            return \random_int(1, 10000);
        } catch (Throwable $e) {
            return \mt_rand(1, 10000);
        }
    }

    /**
     * @param mixed $value
     * @return int
     */
    public static function clamp(mixed $value): int
    {
        if (!\is_numeric($value) || $value < 1) {
            return 1;
        }
        if ($value > 500) {
            return 500;
        }
        return \intval($value);
    }

    /**
     * @param array $rows
     * @return string
     */
    public static function renderFortunes(array $rows): string
    {
        if (self::$fortunesRenderer === null) {
            $code = \file_get_contents(__DIR__ . '/fortunes.php');
            if ($code === false) {
                return '';
            }

            $renderer = static function (array $scope) use ($code): string {
                foreach ($scope as $key => $value) {
                    $$key = $value;
                }
                \ob_start();

                eval('?>' . $code);
                return (string) \ob_get_clean();
            };

            self::$fortunesRenderer = $renderer;
        }

        /** @var callable $renderer */
        $renderer = self::$fortunesRenderer;
        return $renderer(['rows' => $rows]);
    }
}

$manager = new Manager();
$worker = new class () extends Worker {
    /**
     * @var Server
     */
    private Server $server;

    /**
     * @var Channel
     */
    private Channel $STM_queryWorldWhereID;

    /**
     * @var Channel
     */
    private Channel $STM_updateWorldRandomNumber;

    /**
     * @var Channel
     */
    private Channel $STM_queryFortune;

    /**
     * @return void
     */
    public function register(): void
    {
        $this->count = 32;
        $context = \stream_context_create([
            'socket' => [
                'so_reuseport' => 1,
                'so_reuseaddr' => 1
            ]
        ]);
        $this->server = Http::server('http://0.0.0.0:8080', $context);
    }

    /**
     * @return void
     */
    public function boot(): void
    {
        Setup::dateRefresh();
        go(static function () {
            while (1) {
                \Co\sleep(1);
                Setup::dateRefresh();
            }
        });

        $this->STM_queryWorldWhereID = new Channel(10);
        $this->STM_updateWorldRandomNumber = new Channel(10);
        $this->STM_queryFortune = new Channel(10);

        for ($i = 0; $i < 10; $i++) {
            $client = new Client(
                'mysql:host=tfb-database;port=3306;dbname=hello_world',
                'benchmarkdbuser',
                'benchmarkdbpass',
            );

            $this->STM_queryWorldWhereID->send($client->prepare('SELECT id, randomNumber FROM World WHERE id = ?'));
            $this->STM_updateWorldRandomNumber->send($client->prepare('UPDATE World SET randomNumber = ? WHERE id = ?'));
            $this->STM_queryFortune->send($client->prepare('SELECT * FROM `Fortune`'));
        }

        $this->server->onRequest = fn (Request $request) => $this->onRequest($request);
        $this->server->listen();
    }

    /**
     * @param Request $request
     * @return void
     */
    public function onRequest(Request $request): void
    {
        switch ($request->SERVER['REQUEST_URI']) {
            case '/json':
                {
                    $request->respondJson(
                        ['message' => 'Hello, World!'],
                        ['Date' => Setup::$dateFormatted]
                    );
                    break;
                }

            case '/db':
                {
                    $statement = $this->STM_queryWorldWhereID->receive();
                    $statement->execute([Setup::randomInt()]);
                    $request->respondJson($statement->fetch(AbstractResultSet::ASSOC_ARRAY), ['Date' => Setup::$dateFormatted]);
                    $this->STM_queryWorldWhereID->send($statement);
                    break;
                }

            case '/queries':
                {
                    $queries = Setup::clamp($request->GET['queries'] ?? 1);
                    $results = [];

                    $waitGroup = new WaitGroup();
                    $waitGroup->add($queries);

                    while ($queries--) {
                        go(function () use (&$results, $waitGroup) {
                            $statement = $this->STM_queryWorldWhereID->receive();

                            $statement->execute([Setup::randomInt()]);
                            $results[] = $statement->fetch(AbstractResultSet::ASSOC_ARRAY);

                            $this->STM_queryWorldWhereID->send($statement);
                            $waitGroup->done();
                        });
                    }

                    $waitGroup->wait();

                    $request->respondJson($results, ['Date' => Setup::$dateFormatted]);
                    break;
                }

            case '/fortunes':
                {
                    $statement = $this->STM_queryFortune->receive();
                    $statement->execute();
                    $rows = $statement->fetchAll(AbstractResultSet::ASSOC_ARRAY);
                    $this->STM_queryFortune->send($statement);

                    $rows[] = ['id' => 0, 'message' => 'Additional fortune added at request time.'];
                    \usort($rows, function ($a, $b) {
                        return $a['message'] <=> $b['message'];
                    });

                    $request->respondHtml(
                        Setup::renderFortunes($rows),
                        [
                            'Date' => Setup::$dateFormatted,
                            'Content-Type' => 'text/html; charset=UTF-8'
                        ]
                    );
                    break;
                }

            case '/updates':
                {
                    $queries = Setup::clamp($request->GET['queries'] ?? 1);
                    $results = [];

                    $waitGroup = new WaitGroup();
                    $waitGroup->add($queries);

                    while ($queries--) {
                        go(function () use (&$results, $waitGroup) {
                            $statement = $this->STM_queryWorldWhereID->receive();
                            $update = $this->STM_updateWorldRandomNumber->receive();

                            $statement->execute([Setup::randomInt()]);
                            $row = $statement->fetch(AbstractResultSet::ASSOC_ARRAY);
                            $row['randomNumber'] = Setup::randomInt();
                            $results[] = $row;
                            $update->execute([$row['randomNumber'], $row['id']]);

                            $this->STM_queryWorldWhereID->send($statement);
                            $this->STM_updateWorldRandomNumber->send($update);

                            $waitGroup->done();
                        });
                    }

                    $waitGroup->wait();
                    $request->respondJson($results, ['Date' => Setup::$dateFormatted]);

                    break;
                }

            case '/plaintext':
                {
                    $request->respond(
                        'Hello, World!',
                        [
                            'Content-Type' => 'text/plain; charset=utf-8',
                            'Date' => Setup::$dateFormatted
                        ]
                    );
                    break;
                }

            default:
                {
                    $request->respond('Not Found', [], 404);
                }
        }
    }
};

$manager->add($worker);
$manager->run();
wait();
