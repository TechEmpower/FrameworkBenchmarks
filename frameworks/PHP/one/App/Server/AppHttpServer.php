<?php

namespace App\Server;

use App\GlobalData\Client;
use App\Model\World;
use One\Http\Router;
use One\Swoole\Server\HttpServer;

class AppHttpServer extends HttpServer
{
    public $caches = [];

    public function onWorkerStart(\swoole_server $server, $worker_id)
    {
        parent::onWorkerStart($server, $worker_id);
        $arr = World::findAll()->toArray();
        $this->caches = array_column($arr,null,'id');
    }

    public function onRequest(\swoole_http_request $request, \swoole_http_response $response)
    {
        $this->httpRouter($request, $response);
    }
}