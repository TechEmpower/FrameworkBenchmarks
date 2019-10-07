<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\controllers\Startup::$templateEngine = new \Ubiquity\views\engine\micro\MicroTemplateEngine();
\Ubiquity\orm\DAO::setModelsDatabases(["models\\Fortune"=>'worker',"models\\World"=>'worker']);
$workerServer->onWorkerStart=function() use($config){
	\Ubiquity\orm\DAO::startDatabase($config,'worker');
};

