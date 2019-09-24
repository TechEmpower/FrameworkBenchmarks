<?php
require_once __DIR__.'/vendor/autoload.php';

use Workerman\Protocols\Http;
use Workerman\Worker;

$http_worker                = new Worker('http://0.0.0.0:8080');
$http_worker->count         = shell_exec('nproc');
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

$http_worker->onMessage = static function ($connection) {

    global $mysql;

    Http::header('Date: '.gmdate('D, d M Y H:i:s').' GMT');

    switch (parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH)) {
        case '/db':
            Http::header('Content-Type: application/json');
            $mysql->query('SELECT id,randomNumber FROM World WHERE id='.mt_rand(1, 10000), static function ($command, $mysql) use ($connection) {
                $connection->send(json_encode($command->resultRows));
            });
            return;

        case '/fortune':
            Http::header('Content-Type: text/html; charset=utf-8');
            $mysql->query('SELECT id,message FROM Fortune', static function ($command, $mysql) use ($connection) {
                $arr    = $command->resultRows;
                foreach ($arr as $row) {
                    $fortune[$row['id']] = htmlspecialchars($row['message'], ENT_QUOTES, 'UTF-8');
                }
                $fortune[0] = 'Additional fortune added at request time.';
                asort($fortune);

                $html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
                foreach ($fortune as $id => $message) {
                    $html .= "<tr><td>$id</td><td>$message</td></tr>";
                }

                $connection->send($html.'</table></body></html>');
            });
            return;

        //case '/update':
        //    Http::header('Content-Type: application/json');
        //    return $connection->send(update());

        //case '/info':
        //   Http::header('Content-Type: text/plain');
        //   ob_start();
        //   phpinfo();
        //   $connection->send(ob_get_clean());

        //default:
        //   Http::header('HTTP', true, 404);
        //   $connection->send('Error 404');
    }
};

Worker::runAll();
