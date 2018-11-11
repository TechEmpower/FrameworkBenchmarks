<?php

require_once __DIR__ . '/vendor/autoload.php';

$application = new \Benchmark\Application;
\Hamlet\Bootstraps\SwooleBootstrap::run('0.0.0.0', 8080, $application);
