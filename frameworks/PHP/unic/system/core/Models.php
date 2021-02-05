<?php
/**
* Models
* Model Class is handle all the database settings and database transactions.
*
* @package : Model
* @category : System
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class Models {
  /**
  * Connect
  * Initialize database connection manually.
  *
  * @param array $db_name
  * @return void
  */
  protected function connect(...$db_name) {
    $database = new Database();
    //Initialize database connection
    foreach($db_name as $name) {
      $this->$name = $database->connect($name);
    }
  }
}
