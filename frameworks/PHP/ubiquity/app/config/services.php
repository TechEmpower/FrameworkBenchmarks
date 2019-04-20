<?php
\Ubiquity\cache\CacheManager::startProd($config);
\Ubiquity\controllers\Router::start();
/*
try{
	\Ubiquity\orm\DAO::startDatabase($config);
}catch(Exception $e){
	echo $e->getMessage();
}*/
