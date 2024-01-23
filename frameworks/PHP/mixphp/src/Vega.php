<?php

namespace App;

use App\Container\Logger;
use Mix\Vega\Abort;
use Mix\Vega\Context;
use Mix\Vega\Engine;
use Mix\Vega\Exception\NotFoundException;

class Vega
{

    /**
     * @return Engine
     */
    public static function new(): Engine
    {
        $vega = new Engine();
        $vega->mode(Engine::FAST_MODE);

        // 500
        $vega->use(function (Context $ctx) {
            try {
                $ctx->next();
            } catch (\Throwable $ex) {
                if ($ex instanceof Abort || $ex instanceof NotFoundException) {
                    throw $ex;
                }
                Logger::instance()->error(sprintf('%s in %s on line %d', $ex->getMessage(), $ex->getFile(), $ex->getLine()));
                $ctx->string(500, 'Internal Server Error');
                $ctx->abort();
            }
        });

        // views
        $vega->withHTMLRoot(__DIR__ . '/../views');

        // routes
        $routes = require __DIR__ . '/../routes/index.php';
        $routes($vega);

        return $vega;
    }

}
