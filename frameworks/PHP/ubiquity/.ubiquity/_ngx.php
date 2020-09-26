<?php
// ngx.php
if (! defined('DS')) {
	define('DS', DIRECTORY_SEPARATOR);
	define('ROOT', __DIR__ . \DS . '..' . \DS . 'app' . \DS);
}
$config = include ROOT . 'config/config.php';
$config["siteUrl"] = 'http://0.0.0.0:8080';
require ROOT . './../vendor/autoload.php';
\Ubiquity\servers\ngx\NgxServer::init($config);
require ROOT . 'config/ngxServices.php';

function handle() {
	\Ubiquity\servers\ngx\NgxServer::handleRequest();
}