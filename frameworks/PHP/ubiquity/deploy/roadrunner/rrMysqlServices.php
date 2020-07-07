<?php
\Ubiquity\cache\CacheManager::startProd($config);

\Ubiquity\orm\DAO::setModelsDatabases([
	'models\\World' => 'default',
	'models\\Fortune' => 'default'
]);

\Ubiquity\cache\CacheManager::warmUpControllers([
	'controllers\\DbMy',
	'controllers\\Fortunes_'
]);

\Ubiquity\orm\DAO::startDatabase($config, 'default');
\Ubiquity\orm\DAO::prepareGetById('world', 'models\\World');
\Ubiquity\orm\DAO::prepareGetAll('fortune', 'models\\Fortune');

