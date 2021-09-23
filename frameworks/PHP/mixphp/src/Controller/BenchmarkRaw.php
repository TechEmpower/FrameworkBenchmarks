<?php

namespace App\Controller;

use Mix\Vega\Context;

class BenchmarkRaw
{

    public function init()
    {
        global $world, $fortune, $update;
        if (isset($world)) {
            return;
        }

        $pdo = new \PDO(
            'pgsql:host=tfb-database;dbname=hello_world',
            'benchmarkdbuser',
            'benchmarkdbpass',
            [
                \PDO::ATTR_DEFAULT_FETCH_MODE => \PDO::FETCH_ASSOC,
            ]
        );
        $world = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $fortune = $pdo->prepare('SELECT id,message FROM Fortune');
        $update = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
    }

    /**
     * @param Context $ctx
     */
    public function json(Context $ctx)
    {
        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode(['message' => 'Hello, World!']));
    }

    /**
     * @param Context $ctx
     */
    public function plaintext(Context $ctx)
    {
        $ctx->setHeader('Content-Type', 'text/plain; charset=utf-8');
        $ctx->string(200, 'Hello, World!');
    }

    /**
     * @param Context $ctx
     */
    public function db(Context $ctx)
    {
        $this->init();
        global $world;

        $world->execute([mt_rand(1, 10000)]);

        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode($world->fetch()));
    }

    /**
     * @param Context $ctx
     */
    public function query(Context $ctx)
    {
        $this->init();
        global $world;

        $queryCount = 1;
        $q = static::getQuery($ctx);
        if ($q > 1) {
            $queryCount = min($q, 500);
        }

        $arr = [];
        while ($queryCount--) {
            $world->execute([mt_rand(1, 10000)]);
            $arr[] = $world->fetch();
        }

        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode($arr));
    }

    /**
     * @param Context $ctx
     */
    public function fortunes(Context $ctx)
    {
        $this->init();
        global $fortune;

        $fortune->execute();

        $arr = $fortune->fetchAll(\PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        asort($arr);

        $html = '';
        foreach ($arr as $id => $message) {
            $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>$id</td><td>$message</td></tr>";
        }

        $ctx->string(200, "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>");
    }

    /**
     * @param Context $ctx
     */
    public function update(Context $ctx)
    {
        $this->init();
        global $world, $update;

        $queryCount = 1;
        $q = static::getQuery($ctx);
        if ($q > 1) {
            $queryCount = min($q, 500);
        }

        $arr = [];
        while ($queryCount--) {
            $id = mt_rand(1, 10000);
            $world->execute([$id]);
            $ret = $world->fetch();
            $update->execute(
                [$ret['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $arr[] = $ret;
        }

        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode($arr));
    }

    /**
     * @param Context $ctx
     * @return int
     */
    protected static function getQuery(Context $ctx): int
    {
        $request = $ctx->request;
        if ($request instanceof \Swoole\Http\Request) {
            return (int)($request->get['q'] ?? '');
        } elseif ($request instanceof \Workerman\Protocols\Http\Request) {
            return (int)$request->get('q');
        } else {
            return (int)$ctx->query('q');
        }
    }

}
