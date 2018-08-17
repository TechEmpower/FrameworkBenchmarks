<?php

require_once __DIR__ . '/vendor/autoload.php';

use Hamlet\Requests\Request;
use Benchmark\Application;

$application = new Application();
$request = Request::fromGlobals();
$response = $application->run($request);
$application->output($request, $response);

