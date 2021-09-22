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
            $ret = DB::instance()->raw('SELECT id,randomNumber FROM World WHERE id=?', mt_rand(1, 10000))->first();
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
        $rows = DB::instance()->raw('SELECT id,message FROM Fortune')->get();
        $rows[] = (object)['id' => 0, 'message' => 'Additional fortune added at request time.'];
        usort($rows, function ($left, $right) {
            return $left->message <=> $right->message;
        });
        $ctx->HTML(200, 'fortunes', ['rows' => $rows]);
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
            $id = mt_rand(1, 10000);
            $ret = DB::instance()->raw('SELECT id,randomNumber FROM World WHERE id=?', $id)->first();
            DB::instance()->exec('UPDATE World SET randomNumber=? WHERE id=?', $ret->randomNumber = mt_rand(1, 10000), $id);
            $arr[] = $ret;
        }

        $ctx->setHeader('Content-Type', 'application/json');
        $ctx->string(200, json_encode($arr));
    }

}
