<?php

declare(strict_types=1);
/**
 * This file is part of Hyperf.
 *
 * @link     https://www.hyperf.io
 * @document https://doc.hyperf.io
 * @contact  group@hyperf.io
 * @license  https://github.com/hyperf-cloud/hyperf/blob/master/LICENSE
 */

namespace App\Controller;

use App\Model\Fortune;
use App\Model\World;
use App\Render;
use Hyperf\DbConnection\Db;
use Hyperf\Di\Annotation\Inject;
use Hyperf\HttpMessage\Stream\SwooleStream;
use Hyperf\HttpServer\Annotation\Controller;
use Hyperf\HttpServer\Annotation\GetMapping;
use Hyperf\HttpServer\Contract\ResponseInterface;

/**
 * @Controller
 */
class IndexController
{

    /**
     * @Inject()
     * @var Render
     */
    private $render;

    /**
     * @Inject()
     * @var ResponseInterface
     */
    private $response;

    /**
     * @GetMapping(path="/json")
     */
    public function json()
    {
        return $this->response->json(['message' => 'Hello, World!']);
    }

    /**
     * @GetMapping(path="/db")
     */
    public function db()
    {
        return $this->response->json(World::find(random_int(1, 10000)));
    }

    /**
     * @GetMapping(path="/raw-db")
     */
    public function rawDb()
    {
        return $this->response->json(Db::select('SELECT id, randomNumber FROM World WHERE id = ?', [random_int(1, 10000)]));
    }

    /**
     * @GetMapping(path="/queries/[{queries}]")
     */
    public function queries($queries = 1)
    {
        $queries = $this->clamp($queries);

        $rows = [];

        while ($queries--) {
            $rows[] = World::find(random_int(1, 10000));
        }

        return $this->response->json($rows);
    }

    /**
     * @GetMapping(path="/raw-queries/[{queries}]")
     */
    public function rawQueries($queries = 1)
    {
        $queries = $this->clamp($queries);

        $rows = [];

        while ($queries--) {
            $rows[] = Db::selectOne('SELECT id, randomNumber FROM World WHERE id = ?', [random_int(1, 10000)]);
        }

        return $this->response->json($rows);
    }

    /**
     * @GetMapping(path="/fortunes")
     */
    public function fortunes()
    {
        $rows = Fortune::all();

        $insert = new Fortune();
        $insert->id = 0;
        $insert->message = 'Additional fortune added at request time.';

        $rows->add($insert);
        $rows = $rows->sortBy('message');

        return $this->render->render('fortunes', ['rows' => $rows]);
    }

    /**
     * @GetMapping(path="/micro-fortunes")
     */
    public function microFortunes()
    {
        $rows = Db::select('SELECT id, message FROM Fortune');

        $fortune = [];
        foreach ($rows ?? [] as $row) {
            $fortune[$row->id] = $row->message;
        }
        $fortune[0] = 'Additional fortune added at request time.';
        asort($fortune);

        $html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
        foreach ($fortune as $id => $message) {
            $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
        }

        $html .= '</table></body></html>';
        return $this->response->withAddedHeader('content-type', 'text/html; charset=utf-8')->withBody(new SwooleStream($html));
    }

    /**
     * @GetMapping(path="/updates/[{queries}]")
     */
    public function updates($queries = 1)
    {
        $queries = $this->clamp($queries);

        $rows = [];

        while ($queries--) {
            $row = World::find(random_int(1, 10000));
            $row->randomNumber = random_int(1, 10000);
            $row->save();
            $rows[] = $row;
        }

        return $this->response->json($rows);
    }

    /**
     * @GetMapping(path="/raw-updates/[{queries}]")
     */
    public function rawUpdates($queries = 1)
    {
        $queries = $this->clamp($queries);

        $rows = [];

        while ($queries--) {
            $row = Db::selectOne('SELECT id, randomNumber FROM World WHERE id = ?', [$id = random_int(1, 10000)]);
            $rand = random_int(1, 10000);
            $row->randomNumber = $rand;
            Db::update('UPDATE World SET randomNumber = ? WHERE id = ?', [$rand, $id]);
            $rows[] = $row;
        }

        return $this->response->json($rows);
    }

    /**
     * @GetMapping(path="/plaintext")
     */
    public function plaintext()
    {
        return $this->response->raw('Hello, World!');
    }

    private function clamp($value): int
    {
        if (! is_numeric($value) || $value < 1) {
            return 1;
        }
        if ($value > 500) {
            return 500;
        }
        return (int)$value;
    }
}
