<?php
require __DIR__ . '/../vendor/autoload.php';

/**
 * PHP-FPM, cli-server 模式专用
 */

use App\Vega;

App\Error::register();

return Vega::new()->run();
