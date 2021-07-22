<?php

namespace App\Controller;

use App\Container\DB;
use Mix\Vega\Context;

class Benchmark
{

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
        $ctx->setHeader('Content-Type', 'application/json');
        $ret = DB::instance()->raw('SELECT id,randomNumber FROM World WHERE id=?', mt_rand(1, 10000))->first();
        $ctx->string(200, json_encode($ret));
    }

    /**
     * @param Context $ctx
     */
    public function query(Context $ctx)
    {
        $queryCount = 1;
        $q = (int)$ctx->query('q');
        if ($q > 1) {
            $queryCount = min($q, 500);
        }

        $arr = [];
        while ($queryCount--) {
            $id = \mt_rand(1, 10000);
            $ret = DB::instance()->raw('SELECT id,randomNumber FROM World WHERE id=?', $id)->first();
            $arr[] = $ret;
        }

        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode($arr));
    }

    /**
     * @param Context $ctx
     */
    public function fortunes(Context $ctx)
    {
        $fortune = [];
        $arr = DB::instance()->raw('SELECT id,message FROM Fortune')->get();
        foreach ($arr as $row) {
            $fortune[$row['id']] = $row['message'];
        }
        $fortune[0] = 'Additional fortune added at request time.';
        \asort($fortune);

        $html = '';
        foreach ($fortune as $id => $message) {
            $message = \htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
        }
        $ctx->setHeader('Content-Type', 'text/html; charset=utf-8');
        $ctx->string(200, '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            . $html .
            '</table></body></html>');
    }

    /**
     * @param Context $ctx
     */
    public function update(Context $ctx)
    {
        $queryCount = 1;
        $q = (int)$ctx->query('q');
        if ($q > 1) {
            $queryCount = min($q, 500);
        }

        $arr = [];
        while ($queryCount--) {
            $id = \mt_rand(1, 10000);
            $ret = DB::instance()->raw('SELECT id,randomNumber FROM World WHERE id=?', $id)->first();
            DB::instance()->exec('UPDATE World SET randomNumber=? WHERE id=?', $ret['randomNumber'] = mt_rand(1, 10000), $id);
            $arr[] = $ret;
        }

        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode($arr));
    }

}
