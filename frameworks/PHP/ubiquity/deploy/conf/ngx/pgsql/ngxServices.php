<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\Fortune' => 'pgsql',
	'models\\World' => 'pgsql',
	'models\\CachedWorld' => 'pgsql-cache'
]);

\Ubiquity\cache\CacheManager::warmUpControllers([
	\controllers\Plaintext_::class,
	\controllers\Json_::class
	// \controllers\Db_::class,
	// \controllers\Fortunes_::class,
	// \controllers\Cache::class
]);

//\Ubiquity\orm\DAO::startDatabase($config, 'pgsql');
//\controllers\Db_::warmup();
//\controllers\Fortunes_::warmup();
