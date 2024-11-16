<?php declare(strict_types=1);

include __DIR__ . '/vendor/autoload.php';

use Ripple\Http\Server;
use Ripple\Worker\Manager;

use function Co\repeat;
use function Co\wait;

class Setup
{
    public static PDO    $pdo;
    public static string $dateFormatted;

    public static PDOStatement $queryWorldWhereID;
    public static PDOStatement $updateWorldRandomNumber;
    public static PDOStatement $queryFortune;

    public static function dateRefresh(): void
    {
        try {
            $date = new DateTime('now', new DateTimeZone('GMT'));
        } catch (Throwable $e) {
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
            return mt_rand(1, 10000);
        }
    }

    /**
     * @param mixed $value
     *
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
     * @param string $template
     * @param array  $data
     *
     * @return string
     */
    public static function render(string $template, array $data = []): string
    {
        foreach ($data as $key => $value) {
            $$key = $value;
        }

        \ob_start();
        include $template;
        return \ob_get_clean();
    }
}

$manager = new Manager();
$worker  = new class() extends \Ripple\Worker {
    /*** @var \Ripple\Http\Server */
    public Server $server;

    /**
     * @param \Ripple\Worker\Manager $manager
     *
     * @return void
     */
    public function register(Manager $manager): void
    {
        $this->count  = 64;
        $this->server = new Server('http://0.0.0.0:8080');
    }

    /**
     * @return void
     */
    public function boot(): void
    {
        Setup::dateRefresh();
        repeat(static fn () => Setup::dateRefresh(), 1);

        Setup::$pdo = new \PDO(
            'mysql:host=tfb-database;port=3306;dbname=hello_world',
            'benchmarkdbuser',
            'benchmarkdbpass',
            [
                \PDO::ATTR_DEFAULT_FETCH_MODE => \PDO::FETCH_ASSOC,
                \PDO::ATTR_EMULATE_PREPARES   => false,
                \PDO::ATTR_ERRMODE            => \PDO::ERRMODE_EXCEPTION,
            ]
        );

        Setup::$queryWorldWhereID       = Setup::$pdo->prepare('SELECT id, randomNumber FROM World WHERE id = ?');
        Setup::$updateWorldRandomNumber = Setup::$pdo->prepare('UPDATE World SET randomNumber = ? WHERE id = ?');
        Setup::$queryFortune            = Setup::$pdo->prepare('SELECT * FROM `Fortune`');
        $this->server->onRequest(fn (Server\Request $request) => $this->onRequest($request));
        $this->server->listen();
    }

    /**
     * @param \Ripple\Http\Server\Request $request
     *
     * @return void
     */
    public function onRequest(Server\Request $request): void
    {
        switch ($request->SERVER['REQUEST_URI']) {
            case '/json':
                $request->respondJson(
                    ['message' => 'Hello, World!'],
                    ['Date' => Setup::$dateFormatted]
                );
                break;

            case '/db':
                $statement = Setup::$queryWorldWhereID;
                $statement->execute([Setup::randomInt()]);
                $request->respondJson($statement->fetch(), ['Date' => Setup::$dateFormatted]);
                break;

            case '/queries':
                $queries   = Setup::clamp($request->GET['queries'] ?? 1);
                $results   = [];
                $statement = Setup::$queryWorldWhereID;
                while ($queries--) {
                    $statement->execute([Setup::randomInt()]);
                    $results[] = $statement->fetch();
                }
                $request->respondJson($results, ['Date' => Setup::$dateFormatted]);

                break;
            case '/fortunes':
                $rows   = Setup::$pdo->query('SELECT * FROM `Fortune`')?->fetchAll();
                $rows[] = ['id' => 0, 'message' => 'Additional fortune added at request time.'];
                \usort($rows, function ($a, $b) {
                    return $a['message'] <=> $b['message'];
                });

                $request->respondHtml(
                    Setup::render('fortunes.php', ['rows' => $rows]),
                    [
                        'Date'         => Setup::$dateFormatted,
                        'Content-Type' => 'text/html; charset=UTF-8'
                    ]
                );
                break;

            case '/updates':
                $queries   = Setup::clamp($request->GET['queries'] ?? 1);
                $results   = [];
                $statement = Setup::$queryWorldWhereID;
                $update    = Setup::$updateWorldRandomNumber;
                while ($queries--) {
                    $statement->execute([Setup::randomInt()]);
                    $row                 = $statement->fetch();
                    $row['randomNumber'] = Setup::randomInt();
                    $results[]           = $row;
                    $update->execute([$row['randomNumber'], $row['id']]);
                }
                $request->respondJson($results, ['Date' => Setup::$dateFormatted]);
                break;

            case '/plaintext':
                $request->respond(
                    'Hello, World!',
                    [
                        'Content-Type' => 'text/plain; charset=utf-8',
                        'Date'         => Setup::$dateFormatted
                    ]
                );
                break;

            default:
                $request->respond('Not Found', [], 404);
        }
    }
};

$manager->addWorker($worker);
$manager->run();
wait();
