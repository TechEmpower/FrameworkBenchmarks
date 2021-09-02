<?php
use Ubiquity\devtools\cmd\ConsoleFormatter as Console;
use Ubiquity\cache\CacheManager;

//Comments

//For development mode initialization
function _dev($devtools, $config)
{
    echo Console::showInfo("Development mode");
}

//For Production mode initialization
//Create and store dynamic routes.
function _prod($devtools, $config)
{
    echo Console::showInfo("Production mode for benchmark");

    CacheManager::start($config);
    CacheManager::clearCache($config);
    include ROOT.DS.'config'.DS.'microApp.php';
    CacheManager::storeDynamicRoutes(false);
    echo Console::showMessage('Dynamic routes created!', 'success');
    $devtools->run('composer', 'optimize');
}

//Executed before all modes
function bs_before($devtools, $config)
{
}

//Executed after all modes
function bs_after($devtools, $config)
{
    //Initialize all caches
    //$devtools->run('init-cache');
}
