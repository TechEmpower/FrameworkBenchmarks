<?php

require_once __DIR__ . '/vendor/autoload.php';

\Hamlet\Bootstraps\ServerBootstrap::run(function () {
    return new \Benchmark\Application;
});
