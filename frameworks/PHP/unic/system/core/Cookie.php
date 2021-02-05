<?php
/**
* Cookie
* Cookie middleware handle cookies and manage cookies data.
*
* @package : Cookie
* @category : System Middleware
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class Cookie {
  function __construct() {
    if(isset($_COOKIE)) {
      foreach($_COOKIE as $var => $val) {
        $this->$var = &$_COOKIE[$var];
      }
    }
  }

  /**
  * Set Cookie
  *
  * @param string $name
  * @param string $value
  * @param integer $expire
  * @param string $path
  * @param string $domain
  * @param boolean $secure
  * @param boolean $httponly
  * @return boolean
  */
  public function set(string $name, string $value='', int $expire=0, string $path='/', string $domain='', bool $secure=FALSE, bool $httponly=FALSE) {
    if(isset($value)) {
      //Set cookie data
      if(setcookie($name, $value, $expire, $path, $domain, $secure, $httponly)) {
        $this->$name = &$_COOKIE[$name];
        return true;
      } else {
        return false;
      }
    }
  }

  /**
  * Get Cookie
  *
  * @param string $name
  * @return string|void
  */
  public function get(string $name) {
    if(isset($_COOKIE[$name])) {
      return $_COOKIE[$name];
    }
  }

  /**
  * Has Cookie
  *
  * @param string $name
  * @return boolean
  */
  public function has(string $name) {
    if(isset($_COOKIE[$name])) {
      return true;
    } else {
      return false;
    }
  }

  /**
  * Delete Cookie
  *
  * @param string $name
  * @param string $path
  * @param string $domain
  * @return boolean
  */
  public function delete(string $name, string $path='/', string $domain='') {
    if(isset($this->$name)) {
      unset($this->$name);
    }
    return setcookie($name, '', -1, $path, $domain);
  }
}
