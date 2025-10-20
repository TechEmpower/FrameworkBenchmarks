<?php
use Piko\ModularApplication;

require(__DIR__ . '/../vendor/autoload.php');

$config = require __DIR__ . '/../config/app.php';

(new ModularApplication($config))->run();
