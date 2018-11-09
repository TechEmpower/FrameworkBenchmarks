<?php

require_once __DIR__ . '/vendor/autoload.php';

$application = new \Benchmark\Application;
\Hamlet\Bootstraps\AmpBootstrap::run(8080, $application);
