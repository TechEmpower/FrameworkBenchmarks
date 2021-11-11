<?php

namespace App\Server;

use App\GlobalData\Client;
use One\Http\Router;
use One\Swoole\Server\HttpServer;

class AppHttpServer extends HttpServer
{
    public function onRequest(\swoole_http_request $request, \swoole_http_response $response)
    {
        $this->httpRouter($request, $response);
    }
}