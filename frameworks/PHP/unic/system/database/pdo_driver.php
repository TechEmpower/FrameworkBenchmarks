<?php
/**
* PDO Database Driver
* PDO Database Driver extends pdo class and handle all the pdo database connection.
*
* @package : PDO Database Driver
* @category : Driver
* @author : Unic Framework
* @link : https://github.com/unicframework/unic
*/

class pdo_db_driver extends pdo {
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
    $this->username = $db[$name]['username'];
    $this->password = $db[$name]['password'];
    $this->database = $db[$name]['database'];
    $this->hostname = $db[$name]['hostname'];
    $this->port = $db[$name]['port'];
    $this->char_set = $db[$name]['char_set'];
    if($db[$name]['dsn']) {
      $this->dsn = $db[$name]['dsn'];
    } else {
      $this->dsn = 'mysql:host='.$this->hostname.';dbname='.$this->database.';charset='.$this->char_set;
    }

    try {
      parent::__construct($this->dsn, $this->username, $this->password);
      //Set the PDO error mode to exception
      $this->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    } catch(PDOException $error) {
      http_response_code(500);
      throw new Exception('Database Connection Error : '. $error->getMessage());
    }
  }
}
