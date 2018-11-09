<?php

require_once __DIR__ . '/vendor/autoload.php';

$application = new \Benchmark\Application;
\Hamlet\Bootstraps\RoadRunnerBootstrap::run($application);
