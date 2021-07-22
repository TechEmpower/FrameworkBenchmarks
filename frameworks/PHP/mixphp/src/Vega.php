<?php

namespace App;

use Mix\Vega\Engine;

class Vega
{

    /**
     * @return Engine
     */
    public static function new(): Engine
    {
        $vega = new Engine();

        // routes
        $routes = require __DIR__ . '/../routes/index.php';
        $routes($vega);

        return $vega;
    }

}
