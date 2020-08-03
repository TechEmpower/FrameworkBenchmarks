<?php
\Ubiquity\cache\CacheManager::startProd($config);

\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\World' => 'mysql',
	'models\\Fortune' => 'mysql'
]);

\Ubiquity\cache\CacheManager::warmUpControllers([
	\controllers\DbMy::class,
	\controllers\Fortunes_::class
]);

\Ubiquity\orm\DAO::startDatabase($config, 'mysql');
\controllers\DbMy::warmup();
\controllers\Fortunes_::warmup();

