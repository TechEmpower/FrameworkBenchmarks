<?php
$server = new swoole_http_server('0.0.0.0', 8080);
$server->set(array(
    'worker_num' => NUMCORES,
));
$server->on('request', function ($req, $res) {
    $res->header('Content-type', 'application/json');
    $res->end(json_encode(array('message' => 'Hello, World!')));
});
$server->start();