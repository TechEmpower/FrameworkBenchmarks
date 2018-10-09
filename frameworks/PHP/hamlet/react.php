<?php

require_once __DIR__ . '/vendor/autoload.php';

\Hamlet\Bootstraps\ReactBootstrap::run('0.0.0.0:8080', function () {
    return new \Benchmark\Application;
});
