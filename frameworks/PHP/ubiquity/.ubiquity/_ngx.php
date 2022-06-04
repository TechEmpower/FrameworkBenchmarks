<?php
use Ubiquity\servers\ngx\NgxServer;
if (! defined('DS')) {
	define('DS', DIRECTORY_SEPARATOR);
	define('ROOT', __DIR__ . \DS . '..' . \DS . 'app' . \DS);
}
$config = include ROOT . 'config/config.php';
$config["siteUrl"] = 'http://0.0.0.0:8080';
require ROOT . './../vendor/autoload.php';
NgxServer::init($config);
require ROOT . 'config/ngxServices.php';

function handleRequest() {
	NgxServer::handleRequest();
}