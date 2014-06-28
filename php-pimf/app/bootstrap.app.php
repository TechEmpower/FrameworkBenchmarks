<?php
/*
|--------------------------------------------------------------------------
| PIMF bootstrap
|--------------------------------------------------------------------------
*/
if(!defined('DS')) define('DS', DIRECTORY_SEPARATOR, true);
if(!defined('BASE_PATH')) define('BASE_PATH', realpath(dirname(dirname(__FILE__))) . DS, true);

$config = include_once BASE_PATH .'app/config.app.php';

require_once BASE_PATH .'pimf-framework/autoload.core.php';
require_once BASE_PATH .'app/autoload.app.php';
require_once BASE_PATH .'pimf-framework/utils.php';

use \Pimf\Application as App;

App::bootstrap($config, $_SERVER);
