<?php
/**
* Error Handler
* Handle web application errors.
*
* @package : Error Handler
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/fnic
*/

/**
* Debug setting
* Set the error_reporting directive at runtime.
*/
if($debug === FALSE) {
  //Turn off all error reporting.
  error_reporting(0);
  if(function_exists('ini_set')) {
    ini_set('display_errors', 0);
  }
} else {
  //Turn on all error reporting.
  error_reporting(-1);
}

//Set unic error handler
set_error_handler('error_handler');

//Set fatal handler
register_shutdown_function('fatal_handler');

/**
* Error Handler
* server default error handler.
*
* @param integer $errno
* @param string $errstr
* @param string $errfile
* @param integer $errline
* @return boolean
*/
function error_handler($errno, $errstr, $errfile, $errline) {
  global $debug;
  $user_error_handler = parse_errorhandler();
  //Load users error handler
  if(load_error_handler($user_error_handler, $errstr)) {
    return true;
  } else {
    //Check application error debugging
    if($debug === true) {
      //Call php default error handler
      return false;
    } else {
      //Call user's custom errorhandlers
      if(in_array($errno, array(E_ERROR, E_WARNING, E_PARSE, E_CORE_ERROR, E_COMPILE_ERROR, E_USER_ERROR, E_RECOVERABLE_ERROR, E_CORE_WARNING, E_COMPILE_WARNING))) {
        load_error_handler($user_error_handler, 500);
      }
      return true;
    }
  }
}


/**
* Fatal Handler
* server default fatal error handler.
*
* @return void
*/
function fatal_handler() {
  $error = error_get_last();
  if($error && in_array($error['type'], array(E_ERROR, E_CORE_ERROR, E_COMPILE_ERROR, E_USER_ERROR, E_RECOVERABLE_ERROR, E_CORE_WARNING, E_COMPILE_WARNING, E_PARSE))) {
    error_handler($error['type'], $error['message'], $error['file'], $error['line']);
  }
}


/**
* Parse ErrorHandler
* This function configure and parse users custom errorhandler array.
*
* @return array
*/
function parse_errorhandler() {
  global $errorhandlers;
  if(!is_array($errorhandlers)) {
    $errorhandlers = array();
  }
  //Users error handler array
  $user_error_handler = array();
  //Parse errorhandler array
  foreach($errorhandlers as $error => $handler) {
    //Ignore trailing slashes
    $error = trim($error, '/');
    //Check handler is array or not
    if(!is_array($handler)) {
      //Get error handler views class name and method name
      list($class, $method) = explode('.', $handler);
      $user_error_handler[$error]['error'] = $error;
      $user_error_handler[$error]['class'] = $class;
      $user_error_handler[$error]['method'] = $method;
    } else {
      http_response_code(500);
      exit('Error : invalid errorhandler');
    }
  }
  return $user_error_handler;
}

/**
* Load Error Handler
* Handle server error like 404, 500 and load user's custom errorhandler.
*
* @param array $errorhandler
* @param integer $response_code
* @return boolean|void
*/
function load_error_handler($errorhandler, $response_code) {
  global $request;
  //Check user custom errorhandler
  if(array_key_exists($response_code, $errorhandler)) {
    $class = $errorhandler[$response_code]['class'];
    $method = $errorhandler[$response_code]['method'];

    //Set http response code
    if(is_int($response_code)) {
      http_response_code($response_code);
    } else {
      http_response_code(200);
    }

    //Check error handler view class exists or not
    if(class_exists($class)) {
      try {
        //Create error handler view object
        $obj = new $class($request);
        //Check in views class method exists or not
        if(method_exists($obj, $method)) {
          //Call the error handler method
          $obj->$method($request);
          exit();
        } else {
          http_response_code(500);
          exit("Error : '$method' : error handler function does not exists");
        }
      } catch (Exception $e) {
        //Call system default error handler
        return default_error_handler($response_code);
      }
    } else {
      http_response_code(500);
      exit("Error : '$class' : error handler class does not exists");
    }
  } else {
    //Call system default error handler
    return default_error_handler($response_code);
  }
}


/**
* Default Error Handler
* Handle server error like 404, 500 etc.
*
* @param integer $response_code
* @return boolean|void
*/
function default_error_handler($response_code) {
  //Default http response codes
  $status_codes = array(
    100 => 'Continue',
    101 => 'Switching Protocols',

    200 => 'OK',
    201 => 'Created',
    202 => 'Accepted',
    203 => 'Non-Authoritative Information',
    204 => 'No Content',
    205 => 'Reset Content',
    206 => 'Partial Content',

    300 => 'Multiple Choices',
    301 => 'Moved Permanently',
    302 => 'Found',
    303 => 'See Other',
    304 => 'Not Modified',
    305 => 'Use Proxy',
    307 => 'Temporary Redirect',

    400 => 'Bad Request',
    401 => 'Unauthorized',
    402 => 'Payment Required',
    403 => 'Forbidden',
    404 => 'Not Found',
    405 => 'Method Not Allowed',
    406 => 'Not Acceptable',
    407 => 'Proxy Authentication Required',
    408 => 'Request Timeout',
    409 => 'Conflict',
    410 => 'Gone',
    411 => 'Length Required',
    412 => 'Precondition Failed',
    413 => 'Request Entity Too Large',
    414 => 'Request-URI Too Long',
    415 => 'Unsupported Media Type',
    416 => 'Requested Range Not Satisfiable',
    417 => 'Expectation Failed',
    422 => 'Unprocessable Entity',
    426 => 'Upgrade Required',
    428 => 'Precondition Required',
    429 => 'Too Many Requests',
    431 => 'Request Header Fields Too Large',

    500 => 'Internal Server Error',
    501 => 'Not Implemented',
    502 => 'Bad Gateway',
    503 => 'Service Unavailable',
    504 => 'Gateway Timeout',
    505 => 'HTTP Version Not Supported',
    511 => 'Network Authentication Required',
  );
  //System ErrorHandler
  if(array_key_exists($response_code, $status_codes)) {
    //Set http response code
    http_response_code($response_code);
    exit($response_code.' '.$status_codes[$response_code]);
  } else {
    return false;
  }
}
