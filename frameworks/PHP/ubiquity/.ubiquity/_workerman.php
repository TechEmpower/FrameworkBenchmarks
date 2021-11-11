#!/usr/bin/env php
<?php
// workerman.php
if (! defined('DS')) {
	define('DS', DIRECTORY_SEPARATOR);
	define('ROOT', __DIR__ . \DS . '..' . \DS . 'app' . \DS);
}
$config = include ROOT . 'config/config.php';
$sConfig = include __DIR__ . \DS . 'workerman-config.php';
$config["sessionName"] = null;
$address = $sConfig['host'] . ':' . $sConfig['port'];
$config["siteUrl"] = 'http://' . $address;
require ROOT . './../vendor/autoload.php';
$workerServer = new \Ubiquity\servers\workerman\WorkermanServer();
$workerServer->init($config, __DIR__);
$workerServer->setDefaultCount(4);
require ROOT . 'config/workerServices.php';
$workerServer->run($sConfig['host'], $sConfig['port'], $sConfig['socket'] ?? []);
