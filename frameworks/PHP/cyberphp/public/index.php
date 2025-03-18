<?php

declare(strict_types=1);

use Cyber\Response;
use Cyber\App;

require dirname(__DIR__) . '/vendor/autoload.php';


$app = App::bootstrap(dirname(__DIR__) . '/bootstrap.php');

try {
    $response = $app->run();
    if (!$response instanceof Response) {
        $response = Response::html($response ?? '');
    }
    echo $response->send();
} catch (Exception $e) {
    echo renderExceptionPage($e);
} catch (Throwable $e) {
    echo renderExceptionPage($e);
}
