<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\controllers\Startup::$templateEngine = new \Ubiquity\views\engine\micro\MicroTemplateEngine();
\Ubiquity\orm\DAO::setModelsDatabases(["models\\Fortune"=>"swoole","models\\World"=>"swoole"]);
$swooleServer->on('workerStart',function($srv) use($config){
	\Ubiquity\orm\DAO::initPooling($config,'swoole',\intdiv(512,$srv->setting['worker_num']));
});
