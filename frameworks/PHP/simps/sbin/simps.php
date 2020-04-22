#!/usr/bin/env php
<?php
! defined('BASE_PATH') && define('BASE_PATH', dirname(__DIR__, 1));
! defined('CONFIG_PATH') && define('CONFIG_PATH', dirname(__DIR__) . '/config/');

require BASE_PATH . '/vendor/autoload.php';

Simps\Application::run();