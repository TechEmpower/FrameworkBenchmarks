<?php
/**
* System Autoloader
* Autoload framework system files.
*
* @package : Autoloader
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

//Load system files
if(file_exists(SYSPATH.'/core/UrlDispatcher.php')) {
  require_once SYSPATH.'/core/UrlDispatcher.php';
} else {
  http_response_code(500);
  exit('Error : system urls dispatcher not found.');
}

if(file_exists(SYSPATH.'/core/FileHandler.php')) {
  require_once SYSPATH.'/core/FileHandler.php';
} else {
  http_response_code(500);
  exit('Error : system file handler not found.');
}

if(file_exists(SYSPATH.'/core/Session.php')) {
  require_once SYSPATH.'/core/Session.php';
} else {
  http_response_code(500);
  exit('Error : system session file not found.');
}

if(file_exists(SYSPATH.'/core/Cookie.php')) {
  require_once SYSPATH.'/core/Cookie.php';
} else {
  http_response_code(500);
  exit('Error : system cookie file not found.');
}

if(file_exists(SYSPATH.'/core/Request.php')) {
  require_once SYSPATH.'/core/Request.php';
} else {
  http_response_code(500);
  exit('Error : system request file not found.');
}

if(file_exists(SYSPATH.'/core/HttpResponse.php')) {
  require_once SYSPATH.'/core/HttpResponse.php';
} else {
  http_response_code(500);
  exit('Error : system response file not found.');
}

if(file_exists(SYSPATH.'/core/Database.php')) {
  require_once SYSPATH.'/core/Database.php';
} else {
  http_response_code(500);
  exit('Error : system database file not found.');
}

if(file_exists(SYSPATH.'/core/Models.php')) {
  require_once SYSPATH.'/core/Models.php';
} else {
  http_response_code(500);
  exit('Error : system model file not found.');
}

if(file_exists(SYSPATH.'/core/Views.php')) {
  require_once SYSPATH.'/core/Views.php';
} else {
  http_response_code(500);
  exit('Error : system View file not found.');
}

if(file_exists(SYSPATH.'/core/Middlewares.php')) {
  require_once SYSPATH.'/core/Middlewares.php';
} else {
  http_response_code(500);
  exit('Error : system middleware file not found.');
}

if(file_exists(SYSPATH.'/core/Globals.php')) {
  require_once SYSPATH.'/core/Globals.php';
} else {
  http_response_code(500);
  exit('Error : system globals file not found.');
}

if(file_exists(SYSPATH.'/core/Router.php')) {
  require_once SYSPATH.'/core/Router.php';
} else {
  http_response_code(500);
  exit('Error : system router file not found.');
}


//Load application settings file
if(file_exists(BASEPATH.'/application/settings.php')) {
  require_once BASEPATH.'/application/settings.php';
} else {
  http_response_code(500);
  exit('Error : application settings file not found.');
}

//Load application urls file
if(file_exists(BASEPATH.'/application/urls.php')) {
  require_once BASEPATH.'/application/urls.php';
} else {
  http_response_code(500);
  exit('Error : application urls file not found.');
}

//Load system error handler
if(file_exists(SYSPATH.'/core/ErrorHandler.php')) {
  require_once SYSPATH.'/core/ErrorHandler.php';
} else {
  http_response_code(500);
  exit('Error : system error handler file not found.');
}
