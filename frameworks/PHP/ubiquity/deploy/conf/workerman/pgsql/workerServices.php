<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\Fortune' => 'pgsql',
	'models\\World' => 'pgsql',
	'models\\CachedWorld' => 'pgsql-cache'
]);

\Ubiquity\orm\DAO::setCache(new \Ubiquity\cache\dao\DAOMemoryCache());

echo "Loading worlds\n";
\Ubiquity\orm\DAO::warmupCache('models\\CachedWorld', '', false);
echo "End Loading\n";

\Ubiquity\cache\CacheManager::warmUpControllers([
	\controllers\Db_::class,
	\controllers\Fortunes_::class,
	\controllers\Cache::class
]);

$workerServer->onWorkerStart = function () use ($config) {
	\Ubiquity\orm\DAO::startDatabase($config, 'pgsql');
	\controllers\Db_::warmup();
	\controllers\Fortunes_::warmup();
};
