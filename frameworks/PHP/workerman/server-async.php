<?php
require_once __DIR__.'/vendor/autoload.php';

use Workerman\Protocols\Http\Response;
use Workerman\Protocols\Http\Request;
use Workerman\Worker;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = (int) shell_exec('nproc') * 2;
$http_worker->onWorkerStart = static function() {
    global $mysql;

    $loop  = Worker::getEventLoop();

    $mysql = new React\MySQL\Connection($loop, [
        'host'   => 'tfb-database',
        'dbname' => 'hello_world',
        'user'   => 'benchmarkdbuser',
        'passwd' => 'benchmarkdbpass'
    ]);

    $mysql->on('error', function($e){
        echo $e;
    });
   
    $mysql->connect(function ($e) {});
};

$http_worker->onMessage = static function ($connection, $request) {

    global $mysql;

    switch ($request->path()) {
        case '/db':
            $mysql->query('SELECT id,randomNumber FROM World WHERE id='.mt_rand(1, 10000),
                static function ($command) use ($connection) {
                    $connection->send(new Response(200, ['Content-Type' => 'application/json', 'Date' => gmdate('D, d M Y H:i:s').' GMT'], json_encode($command->resultRows, JSON_NUMERIC_CHECK)));
                }
            );
            return;

        case '/fortunes':
            // By default use 'Content-Type: text/html; charset=utf-8';
            $mysql->query('SELECT id,message FROM Fortune', 
                static function ($command) use ($connection) {
                    $arr = $command->resultRows;
                    foreach ($arr as $row) {
                        $fortune[$row['id']] = htmlspecialchars($row['message'], ENT_QUOTES, 'UTF-8');
                    }
                    $fortune[0] = 'Additional fortune added at request time.';
                    asort($fortune);

                    $html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
                    foreach ($fortune as $id => $message) {
                        $html .= "<tr><td>$id</td><td>$message</td></tr>";
                    }

                    $connection->send(new Response(200, ['Date' => gmdate('D, d M Y H:i:s').' GMT'], $html.'</table></body></html>'));

                }
            );
            return;

        //case '/update':
        //    Http::header('Content-Type: application/json');
        //    return $connection->send(update());

        //case '/info':
        //   Http::header('Content-Type: text/plain');
        //   ob_start();
        //   phpinfo();
        //   return $connection->send(ob_get_clean());

        default:
            $connection->send(new Response(200, [], 'Error 404'));

    }
};

Worker::runAll();
