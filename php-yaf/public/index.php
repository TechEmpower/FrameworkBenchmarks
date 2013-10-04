<?php

define('APP_PATH', __DIR__ . '/..');
$app = new Yaf\Application(APP_PATH . '/app/conf/application.ini', 'product');
$app->bootstrap()->run();