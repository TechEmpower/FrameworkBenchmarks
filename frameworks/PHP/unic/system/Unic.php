<?php
/**
* Unic Framework
*
* A high performance, open source web application framework.
*
* @package : Unic Framework
* @author : Rajkumar Dusad
* @copyright : Unic Framework
* @license : MIT License
* @link : https://github.com/unicframework/unic
*/

//System path
define('SYSPATH', rtrim(__DIR__, '/'));

//Application base path
define('BASEPATH', rtrim(dirname(__DIR__), '/'));

//Include system autoloader
require_once SYSPATH.'/core/Autoloader.php';

//Include composer autoloader
if(file_exists(BASEPATH.'/vendor/autoload.php')) {
  require_once BASEPATH.'/vendor/autoload.php';
}

/*
* Unic Framework
* Initialize web application and handle request.
*
* @package : Unic
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/
class Unic {
  function __construct() {
    //Create router object
    $this->router = new Router();
  }

  /**
  * Run web application
  * Listen and handle server requests.
  */
  function run() {
    //Handle server request
    $this->router->handle_request();
  }
}
