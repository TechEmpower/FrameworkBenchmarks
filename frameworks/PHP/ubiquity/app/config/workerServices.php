<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\controllers\Startup::$templateEngine = new \Ubiquity\views\engine\micro\MicroTemplateEngine();
\Ubiquity\orm\DAO::setModelsDatabases(["models\\Fortune"=>"worker","models\\World"=>"worker"]);
