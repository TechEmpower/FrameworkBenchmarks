#!/usr/bin/env php
<?php
// swoole.php

if (! defined ( 'DS' )) {
	define ( 'DS', DIRECTORY_SEPARATOR );
	define ( 'ROOT', __DIR__ . \DS .'..'.\DS. 'app' . \DS );
}
$config=include ROOT.'config/config.php';
$sConfig= include __DIR__.\DS.'swoole-config.php';
$config["sessionName"]=null;
$address=$sConfig['host'].':'.$sConfig['port'];
$config ["siteUrl"] = 'http://'.$address;
require ROOT . './../vendor/autoload.php';
$swooleServer=new \Ubiquity\servers\swoole\SwooleServer();
$swooleServer->init($config, __DIR__);
require ROOT.'config/services.php';
$swooleServer->run($sConfig['host'],$sConfig['port']);