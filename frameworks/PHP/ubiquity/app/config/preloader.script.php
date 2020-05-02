<?php
define('APP_ROOT', __DIR__ . '/../');
include_once \APP_ROOT . './../vendor/phpmv/ubiquity/src/Ubiquity/cache/preloading/PreloaderInternalTrait.php';
include_once \APP_ROOT . './../vendor/phpmv/ubiquity/src/Ubiquity/cache/Preloader.php';
$config = include __DIR__ . '/preloader.config.php';
\Ubiquity\cache\Preloader::fromArray(\APP_ROOT, $config);
