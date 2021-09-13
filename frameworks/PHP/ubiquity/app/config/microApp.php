<?php
use Ubiquity\controllers\Router;

/**
 * **************************
 * Ubiquity micro application
 * **************************
 */

Router::get('micro/json', function () {
    \Ubiquity\utils\http\UResponse::setContentType('application/json');
    echo \json_encode([
        'message' => 'Hello, World!'
    ]);
});

Router::get('micro/plaintext', function () {
    \Ubiquity\utils\http\UResponse::setContentType('text/plain', 'utf-8');
    echo 'Hello, World!';
});

