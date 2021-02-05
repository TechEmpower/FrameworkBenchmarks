<?php
/**
* Files Handler
* file_handler class store all uploaded files data and upload files on the server.
*
* @package : File Handler
* @category : System Middleware
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class FileHandler {
  function __construct() {
    if(isset($_FILES)) {
      foreach($_FILES as $var => $val) {
        $this->$var = &$_FILES[$var];
      }
    }
  }

  /**
  * Has File
  *
  * @param string $name
  * @return boolean
  */
  public function has(string $name) {
    if(isset($_FILES[$name])) {
      return true;
    } else {
      return false;
    }
  }

  /**
  * Upload files on the server.
  *
  * @param string $tmp_name
  * @param string $destination
  * @return boolean
  */
  public function upload(string $tmp_name, string $destination) {
    return move_uploaded_file($tmp_name, $destination);
  }
}
