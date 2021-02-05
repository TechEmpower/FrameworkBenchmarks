<?php
/**
* Session
* Session middleware handle session and manage sessions data.
*
* @package : Session
* @category : System Middleware
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class Session {
  public function __construct() {
    //Start session
    if(!session_id()) {
      session_start();
    }
    if(isset($_SESSION)) {
      foreach($_SESSION as $var => $val) {
        $this->$var = &$_SESSION[$var];
      }
    }
  }

  /**
  * Set Session
  *
  * @param string $name
  * @param string $value
  * @return void
  */
  public function set(string $name, $value='') {
    if(isset($value)) {
      $_SESSION[$name] = $value;
      $this->$name = &$_SESSION[$name];
    }
  }

  /**
  * Get Session
  *
  * @param string $name
  * @return string|void
  */
  public function get(string $name) {
    if(isset($_SESSION[$name])) {
      return $_SESSION[$name];
    }
  }

  /**
  * Has Session
  *
  * @param string $name
  * @return boolean
  */
  public function has(string $name) {
    if(isset($_SESSION[$name])) {
      return true;
    } else {
      return false;
    }
  }

  /**
  * Delete Session
  *
  * @param string $data
  * @return void
  */
  public function delete(string $name) {
    if(isset($this->$name)) {
      unset($this->$name);
    }
    if(isset($_SESSION[$name])) {
      unset($_SESSION[$name]);
    }
  }

  /**
  * Destroy Session
  *
  * @return void
  */
  public function destroy() {
    session_destroy();
  }
}
