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
use Hyperf\HttpMessage\Stream\SwooleStream;
use Hyperf\HttpServer\Annotation\Controller;
use Hyperf\HttpServer\Annotation\GetMapping;
use Hyperf\HttpServer\Contract\RequestInterface;
use Hyperf\HttpServer\Contract\ResponseInterface;
use Hyperf\Utils\Context;

/**
 * @Controller
 */
class IndexController
{
    /**
     * @GetMapping(path="/json")
     */
    public function json()
    {
        return ['message' => 'Hello, World!'];
    }

    /**
     * @GetMapping(path="/db")
     */
    public function db()
    {
        return World::find(random_int(1, 10000));
    }

    /**
     * @GetMapping(path="/queries/[{queries}]")
     */
    public function queries($queries = 1, ResponseInterface $response)
    {
        $queries = $this->clamp($queries);

        $rows = [];
        while ($queries--) {
            $rows[] = World::find(random_int(1, 10000))->toArray();
        }

        return $response->json($rows);
    }

    /**
     * @GetMapping(path="/fortunes")
     */
    public function fortunes(ResponseInterface $response)
    {
        $rows = Fortune::all();

        $insert = new Fortune();
        $insert->id = 0;
        $insert->message = 'Additional fortune added at request time.';

        $rows->add($insert);
        $rows = $rows->sortBy('message');

        $html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
        /** @var Fortune $fortune */
        foreach ($rows as $fortune) {
            $message = htmlspecialchars((string)$fortune->message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>{$fortune->id}</td><td>{$message}</td></tr>";
        }

        $html .= '</table></body></html>';
        return $response->withAddedHeader('content-type', 'text/html; charset=utf-8')->withStatus(200)->withBody(new SwooleStream($html));
    }

    /**
     * @GetMapping(path="/updates/[{queries}]")
     */
    public function updates($queries = 1, ResponseInterface $response)
    {
        $queries = $this->clamp($queries);

        $rows = [];

        while ($queries--) {
            $row = World::find(random_int(1, 10000));
            $row->randomNumber = random_int(1, 10000);
            $row->save();

            $rows[] = $row->toArray();
        }

        return $response->json($rows);
    }

    /**
     * @GetMapping(path="/plaintext")
     */
    public function plaintext()
    {
        return 'Hello, World!';
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
