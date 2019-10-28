<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	"models\\Fortune" => 'pgsql',
	"models\\World" => 'pgsql'
]);
$swooleServer->on('workerStart', function ($srv) use (&$config) {
	\Ubiquity\orm\DAO::startDatabase($config, 'pgsql');
});
