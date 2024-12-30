<?php

use Swoole\Database\PDOConfig;
use Swoole\Database\PDOPool;
class MysqlSwoole
{

    protected Pool|PDOPool $pool;

    public function __construct($size)
    {
        $config = (new PDOConfig())
            ->withDriver('mysql')
            ->withHost('tfb-database')
            ->withPort(3306)
            ->withDbName('hello_world')
            ->withUsername('benchmarkdbuser')
            ->withPassword('benchmarkdbpass');
        $this->pool = new PDOPool($config, $size);
    }

    function db(): array
    {
        $pdo = $this->pool->get();
        $stmt = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $stmt->execute([mt_rand(1, 10000)]);
        $result = $stmt->fetch(PDO::FETCH_ASSOC);
        $this->pool->put($pdo);
        return $result;
    }

    function query($request): array
    {
        $count = min(max((int) $request->get('q'), 1), 500);
        $pdo = $this->pool->get();
        $stmt = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $arr = [];
        while ($count--) {
            $stmt->execute([mt_rand(1, 10000)]);
            $arr[] = $stmt->fetch(PDO::FETCH_ASSOC);
        }
        $this->pool->put($pdo);
        return $arr;
    }

    function update($request): array
    {
        $count = min(max((int) $request->get('q'), 1), 500);
        $arr = [];
        $pdo = $this->pool->get();
        $world = $pdo->prepare('SELECT id,randomNumber FROM World WHERE id=?');
        $update = $pdo->prepare('UPDATE World SET randomNumber=? WHERE id=?');
        while ($count--) {
            $id = mt_rand(1, 10000);
            $world->execute([$id]);
            $item = $world->fetch(PDO::FETCH_ASSOC);
            $update->execute(
                [$item['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $arr[] = $item;
        }
        $this->pool->put($pdo);
        return $arr;
    }

    function fortune(): string
    {
        $pdo = $this->pool->get();
        $stmt = $pdo->prepare('SELECT id,message FROM Fortune');
        $stmt->execute();
        $arr = $stmt->fetchAll(PDO::FETCH_KEY_PAIR);
        $this->pool->put($pdo);
        $arr[0] = 'Additional fortune added at request time.';
        asort($arr);
        $html = '';
        foreach ($arr as $id => $message) {
            $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>$id</td><td>$message</td></tr>";
        }
        return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>$html</table></body></html>";
    }

}