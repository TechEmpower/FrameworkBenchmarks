<?php
/**
* MySQLi Database Driver
* MySQLi Database Driver extends mysqli class and handle mysql database connection.
*
* @package : MySQLi Database Driver
* @category : Driver
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class mysqli_db_driver extends mysqli {
  //DSN
  protected $dsn;
  //User Name
  protected $username;
  //Password
  protected $password;
  //Database
  protected $database;
  //Server hostname
  protected $hostname;
  //Server Port
  protected $port;
  //Charset
  protected $char_set;

  function __construct($db, $name) {
    $this->dsn = $db[$name]['dsn'];
    $this->username = $db[$name]['username'];
    $this->password = $db[$name]['password'];
    $this->database = $db[$name]['database'];
    $this->hostname = $db[$name]['hostname'];
    $this->port = $db[$name]['port'];
    $this->char_set = $db[$name]['char_set'];

    //Create MySQLi database connection
    parent::__construct($this->hostname, $this->username, $this->password,$this->database, $this->port);

    //Check connection error
    if($this->connect_error) {
      http_response_code(500);
      throw new Exception('Database Connection Error ('.$this->connect_errno.') '.$this->connect_error);
    } else {
      //Set charset
      $this->set_charset($this->char_set);
    }
  }
}
