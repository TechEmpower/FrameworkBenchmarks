<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\Fortune' => 'pgsql',
	'models\\World' => 'pgsql',
	'models\\CachedWorld'=>'pgsql-cache'
]);

\Ubiquity\orm\DAO::setCache(new \Ubiquity\cache\dao\DAOMemoryCache());

echo "Loading worlds\n";
\Ubiquity\orm\DAO::warmupCache('models\\CachedWorld','',false);
echo "End Loading worlds\n";

\Ubiquity\cache\CacheManager::warmUpControllers([
	'controllers\\Plaintext_',
	'controllers\\Json_',
	'controllers\\DbPg',
	'controllers\\Fortunes_',
	'controllers\\Cache'
]);
$workerServer->onWorkerStart = function () use ($config) {
	\Ubiquity\orm\DAO::startDatabase($config, 'pgsql');
	\Ubiquity\orm\DAO::prepareGetById('world', 'models\\World');
	\Ubiquity\orm\DAO::prepareGetAll('fortune', 'models\\Fortune');
};

