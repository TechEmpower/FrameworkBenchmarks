<?php

declare(strict_types=1);

use App\App;

\mb_internal_encoding('UTF-8');
\error_reporting(E_ALL | E_STRICT ^ E_DEPRECATED);
\ini_set('display_errors', 'stderr');

// Register Composer's auto loader.
require __DIR__ . '/vendor/autoload.php';

// Initialize shared container, bindings, directories and etc.
$app = App::create(directories: ['root' => __DIR__])->run();

if ($app === null) {
    exit(255);
}

$code = (int)$app->serve();
exit($code);
