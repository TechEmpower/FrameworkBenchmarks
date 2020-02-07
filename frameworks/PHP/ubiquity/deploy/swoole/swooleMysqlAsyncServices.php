<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\Fortune' => 'async',
	'models\\World' => 'async'
]);
\Ubiquity\cache\CacheManager::warmUpControllers([
	'controllers\\SwooleDbAsync',
	'controllers\\SwooleFortunesAsync'
]);
$swooleServer->on('workerStart', function ($srv) use (&$config) {
	\Ubiquity\orm\DAO::initPooling($config, 'async', \intdiv(512, $srv->setting['worker_num']));
	\Ubiquity\orm\DAO::prepareGetById('world', 'models\\World');
	\Ubiquity\orm\DAO::prepareGetAll('fortune', 'models\\Fortune');
});
