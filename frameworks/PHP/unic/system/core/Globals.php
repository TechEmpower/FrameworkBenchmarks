<?php
/**
* Global
* Global variables and methods.
*
* @package : Global
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

if(file_exists(SYSPATH.'/core/Security.php')) {
  require_once SYSPATH.'/core/Security.php';
} else {
  http_response_code(500);
  exit('Error : system security file not found.');
}

//Global request object
$request = NULL;

/**
* Include urlpatterns array
*
* @param array $urls
* @return array
*/
function urls($urls) : array {
  if(file_exists(BASEPATH.'/application/'.trim($urls, '/'))) {
    include_once(BASEPATH.'/application/'.trim($urls, '/'));
    if(isset($urlpatterns) && is_array($urlpatterns)) {
      return $urlpatterns;
    } else {
      http_response_code(500);
      exit('Error : '.$urls.' invalid urlpatterns format');
    }
  } else {
    http_response_code(500);
    exit('Error : '.$urls.' file not found');
  }
}

  /**
  * Base URL
  * Base URL generate the absolute url of any path and it will also return site base url.
  *
  * @param string $path
  * @return string
  */
function url(string $path=NULL) {
  $base_url = ((isset($_SERVER['HTTPS']) && ($_SERVER['HTTPS'] === 'on' || $_SERVER['HTTPS'] === 1)) || (isset($_SERVER['HTTP_X_FORWARDED_PROTO']) && $_SERVER['HTTP_X_FORWARDED_PROTO'] === 'https') || (isset($_SERVER['HTTP_FRONT_END_HTTPS']) && strtolower($_SERVER['HTTP_FRONT_END_HTTPS'])!== 'off') ? 'https' : 'http').'://'.$_SERVER['HTTP_HOST'].'/';
  if(isset($path)) {
    $path = ltrim($path, '/');
    return rtrim($base_url, '/').'/'.$path;
  } else {
    return $base_url;
  }
}

  /**
  * Static URL
  * Static URL return static files URL and generate absolute URL of any path.
  *
  * @param string $path
  * @retutn string
  */

function static_url(string $path=NULL) {
  global $static_url;
  $static = rtrim(url(), '/').'/'.ltrim($static_url, '/');
  if(isset($path)) {
    $path = ltrim($path, '/');
    return rtrim($static, '/').'/'.$path;
  } else {
    return $static;
  }
}

  /**
  * Raise
  * Raise user defined custom error.
  *
  * @param string $errstr
  * @param integer $error
  * @return void
  */
function raise(string $errstr, $error = E_USER_ERROR) {
  trigger_error($errstr, $error);
}

/**
* DB
* DB handle all database connection.
*
* @param string $db_name
* @return mixed
*/
function DB(string $db_name) {
  $database = new Database();
  return $database->connect($db_name);
}

/**
* Set Lang
* Set default local language.
*
* @param string $lang
* @return void
*/
function set_lang(string $lang) {
  global $default_language;
  $default_language = $lang;
}

/**
* Get Lang
* Get application local language.
*
* @return string|void
*/
function get_lang() {
  global $default_language;
  return $default_language;
}

/**
* Lang
* Lang is used for localisation.
*
* @param string $lang_var
* @return mixed
*/
function lang(string $lang_var) {
  global $templates, $default_language;
  if(!isset($default_language) || empty($default_language)) {
    $default_language = 'en';
  }

  //Get application template directory.
  if(isset($templates) && !empty($templates)) {
    if(!is_array($templates)) {
      $templates = array($templates);
    }
  } else {
    $templates = array();
  }

  //Parse lang var.
  if(preg_match("/\./i", $lang_var)) {
    //Parse lang values form custom lang file
    list($lang_file, $var) = explode('.', $lang_var);
    //Check lang file exists or not.
    foreach($templates as $template_dir) {
      //Get template directory path
      $template_path = BASEPATH.'/application/'.trim($template_dir, '/').'/lang/'.$default_language;
      //Get template path.
      if(is_dir($template_path) && is_file($template_path.'/'.$lang_file.'.json')) {
        $lang_path = $template_path.'/'.$lang_file.'.json';
      }
    }
  } else {
    $lang_file = $default_language;
    $var = $lang_var;
    //Check lang file exists or not.
    foreach($templates as $template_dir) {
      //Get template directory path
      $template_path = BASEPATH.'/application/'.trim($template_dir, '/').'/lang/'.$default_language;
      //Get template path.
      if(is_file($template_path.'.json')) {
        $lang_path = $template_path.'.json';
      }
    }
  }

  //Return lang value.
  if(isset($lang_path)) {
    $json_data = file_get_contents($lang_path);
    $values = !empty($json_data) ? json_decode($json_data, true) : array();
    return is_array($values) && isset($values[$var]) ? $values[$var] : NULL;
  } else {
    http_response_code(500);
    exit("Error : '$lang_file' lang file not found");
  }
}

/**
* ENV
*
* @param string $env_var
* @return mixed
*/
function env(string $env_var) {
  if(array_key_exists($env_var, $_ENV)) {
    return $_ENV[$env_var];
  } else if(array_key_exists($env_var, $_SERVER)) {
    return $_SERVER[$env_var];
  } else {
    //Parse .env file
    $path = BASEPATH.'/.env';
    if(is_readable($path)) {
      $lines = file($path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
      foreach($lines as $line) {
        if(strpos(trim($line), '#') === 0) {
          continue;
        }
        list($name, $value) = explode('=', $line, 2);
        $name = trim($name);
        $value = trim($value);
        if(!array_key_exists($name, $_SERVER) && !array_key_exists($name, $_ENV)) {
          putenv(sprintf('%s=%s', $name, $value));
          $_ENV[$name] = $value;
          $_SERVER[$name] = $value;
        }
      }
      return getenv($env_var);
    } else {
      return NULL;
    }
  }
}