<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	"models\\Fortune" => 'pgsql',
	"models\\World" => 'pgsql'
]);
\Ubiquity\cache\CacheManager::warmUpControllers([
	'controllers\\Plaintext',
	'controllers\\Json',
	'controllers\\WorkerDb',
	'controllers\\WorkerFortunes'
]);
$workerServer->onWorkerStart = function () use ($config) {
	\Ubiquity\orm\DAO::startDatabase($config, 'pgsql');
};

