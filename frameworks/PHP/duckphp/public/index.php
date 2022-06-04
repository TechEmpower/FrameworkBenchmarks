<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */
require __DIR__.'/../vendor/autoload.php';


$options = [
    //'is_debug'=>true,
];

\DuckPhpBenchmark\System\App::RunQuickly($options);
