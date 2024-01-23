<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAONosql::setModelsDatabases([
	'models\\Fortune' => 'mongo',
	'models\\World' => 'mongo'
]);

\Ubiquity\cache\CacheManager::warmUpControllers([
	\controllers\DbMongo::class,
	\controllers\FortunesMongo::class
]);

$workerServer->onWorkerStart = function () use ($config) {
	\Ubiquity\orm\DAONosql::startDatabase($config, 'mongo');
	\controllers\DbMongo::warmup();
	\controllers\FortunesMongo::warmup();
};

