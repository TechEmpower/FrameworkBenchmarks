<?php
\One\Database\Mysql\Connect::setConfig(config('mysql', true));
\One\Http\Router::setConfig(['path' => _APP_PATH_ . '/Config/router.php']);
\One\Exceptions\Handler::setConfig(config('exception', true));
\One\Swoole\OneServer::setConfig(config(env('server', 'protocol'), true));

\One\Log::setConfig(config('log', true));

\One\Cache\File::setConfig(config('cache.file', true));

\One\Http\Router::loadRouter();




