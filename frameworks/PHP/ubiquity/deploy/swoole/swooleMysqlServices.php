<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\World' => 'default'
]);
\Ubiquity\cache\CacheManager::warmUpControllers([
	'controllers\\DbMy'
]);
$swooleServer->on('workerStart', function ($srv) use (&$config) {
	\Ubiquity\orm\DAO::startDatabase($config, 'default');
	\Ubiquity\orm\DAO::prepareGetById('world', 'models\\World');
});
