<?php

use Swoole\Database\PDOConfig;
use Swoole\Database\PDOPool;
class PgsqlSwoole extends MysqlSwoole
{

    protected Pool|PDOPool $pool;

    public function __construct($size)
    {
        $config = (new PDOConfig())
            ->withDriver('pgsql')
            ->withHost('tfb-database')
            ->withPort(5432)
            ->withDbName('hello_world')
            ->withUsername('benchmarkdbuser')
            ->withPassword('benchmarkdbpass');

        $this->pool = new PDOPool($config, $size);
    }


    function update($request): array
    {
        $count = min(max((int) $request->get('q'), 1), 500);
        $worlds = [];
        $pdo = $this->pool->get();
        $random = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        while ($count--) {
            $random->execute([mt_rand(1, 10000)]);
            $world = $random->fetch(PDO::FETCH_ASSOC);
            $world['randomNumber'] = mt_rand(1, 10000);
            $worlds[] = $world;
        }
        $rows = count($worlds);

        $sql = 'UPDATE world SET randomNumber = CASE id'
            . str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $rows)
            . 'END WHERE id IN ('
            . str_repeat('?::INTEGER,', $rows - 1) . '?::INTEGER)';

        $update = $pdo->prepare($sql);

        $val = [];
        $keys = [];
        foreach ($worlds as $world) {
            $val[] = $keys[] = $world['id'];
            $val[] = $world['randomNumber'];
        }

        $update->execute([...$val, ...$keys]);
        $this->pool->put($pdo);
        return $worlds;
    }

}