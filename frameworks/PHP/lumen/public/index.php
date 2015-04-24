<?php

$app = require __DIR__.'/../bootstrap/app.php';

echo "~~~~~~~~~~~~~~~ APP VAR DUMP ~~~~~~~~~~~~~";
var_dump($app);

$app->run();

