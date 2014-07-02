<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

/**
 * Redis usage
 *
 * <code>
 *    // Get the default Redis database instance
 *    $redis = Redis::db();
 *
 *    // Get a specified Redis database instance
 *    $reids = Redis::db('redis_2');
 *
 *    // Execute the GET command for the "name" key
 *    $name = Redis::db()->run('get', array('name'));
 *
 *    // Execute the LRANGE command for the "list" key
 *    $list = Redis::db()->run('lrange', array(0, 5));
 *
 * </code>
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 * @method expire($key, $seconds)
 * @method set($key, $value)
 * @method del($key)
 * @method forget($key)
 * @method get($key)
 * @method select($database_id)
 * @method put($session_id, $session, $lifetime);
 */
class Redis
{
  /**
   * The address for the Redis host.
   *
   * @var string
   */
  protected $host;

  /**
   * The port on which Redis can be accessed on the host.
   *
   * @var int
   */
  protected $port;

  /**
   * The database number the connection selects on load.
   *
   * @var int
   */
  protected $database;

  /**
   * The connection to the Redis database.
   *
   * @var resource
   */
  protected $connection;

  /**
   * The active Redis database instances.
   *
   * @var array
   */
  protected static $databases = array();

  /**
   * Create a new Redis connection instance.
   *
   * @param string $host
   * @param int    $port
   * @param int    $database
   */
  public function __construct($host, $port, $database = 0)
  {
    $this->host = $host;
    $this->port = $port;
    $this->database = $database;
  }

  /**
   * Get a Redis database connection instance.
   *
   * The given name should correspond to a Redis database in the configuration file.
   *
   * @param string $name
   *
   * @return Redis
   * @throws \RuntimeException
   */
  public static function database($name = 'default')
  {
    if (!isset(static::$databases[$name])) {
      $conf = Registry::get('conf');

      if (!isset($conf['cache']['storage']) || $conf['cache']['storage'] != 'redis') {
        throw new \RuntimeException("Redis database [$name] is not defined.");
      }

      static::$databases[$name]
        = new static($conf['cache']['server']['host'], $conf['cache']['server']['port'], $conf['cache']['server']['database']);
    }

    return static::$databases[$name];
  }

  /**
   * Execute a command against the Redis database.
   *
   * @param string $method
   * @param array  $parameters
   *
   * @return mixed
   */
  public function run($method, $parameters)
  {
    fwrite($this->connect(), $this->command($method, (array)$parameters));

    $response = trim(fgets($this->connection, 512));

    return $this->parse($response);
  }

  /**
   * Parse and return the response from the Redis database.
   *
   * @param string $response
   *
   * @return array|string
   * @throws \RuntimeException
   */
  protected function parse($response)
  {
    switch (substr($response, 0, 1)) {
      case '-':
        throw new \RuntimeException('Redis error: ' . substr(trim($response), 4));

      case '+':
      case ':':
        return $this->inline($response);

      case '$':
        return $this->bulk($response);

      case '*':
        return $this->multibulk($response);

      default:
        throw new \RuntimeException("Unknown Redis response: " . substr($response, 0, 1));
    }
  }

  /**
   * Establish the connection to the Redis database.
   *
   * @return resource
   * @throws \RuntimeException
   */
  protected function connect()
  {
    if (!is_null($this->connection)) {
      return $this->connection;
    }

    $this->connection = @fsockopen($this->host, $this->port, $error, $message);

    if ($this->connection === false) {
      throw new \RuntimeException("Error making Redis connection: {$error} - {$message}");
    }

    $this->select($this->database);

    return $this->connection;
  }

  /**
   * Build the Redis command based from a given method and parameters.
   *
   * Redis protocol states that a command should conform to the following format:
   *
   *     *<number of arguments> CR LF
   *     $<number of bytes of argument 1> CR LF
   *     <argument data> CR LF
   *     ...
   *     $<number of bytes of argument N> CR LF
   *     <argument data> CR LF
   *
   * More information regarding the Redis protocol: http://redis.io/topics/protocol
   *
   * @param string $method
   * @param $parameters
   *
   * @return string
   */
  protected function command($method, $parameters)
  {
    $CRLF = "\r\n";

    $command = '*' . (count($parameters) + 1) . $CRLF . '$' . strlen($method) . $CRLF . strtoupper($method) . $CRLF;

    foreach ($parameters as $parameter) {
      $command .= '$' . strlen($parameter) . $CRLF . $parameter . $CRLF;
    }

    return $command;
  }

  /**
   * Parse and handle an inline response from the Redis database.
   *
   * @param string $response
   *
   * @return string
   */
  protected function inline($response)
  {
    return substr(trim($response), 1);
  }

  /**
   * Parse and handle a bulk response from the Redis database.
   *
   * @param string $head
   *
   * @return string
   */
  protected function bulk($head)
  {
    if ($head == '$-1') {
      return null;
    }

    list($read, $response, $size) = array(0, '', substr($head, 1));

    if ($size > 0) {
      do {

        // Calculate and read the appropriate bytes off of the Redis response.
        $block = (($remaining = $size - $read) < 1024) ? $remaining : 1024;
        $response .= fread($this->connection, $block);
        $read += $block;

      } while ($read < $size);
    }

    // The response ends with a trailing CRLF.
    fread($this->connection, 2);

    return $response;
  }

  /**
   * Parse and handle a multi-bulk reply from the Redis database.
   *
   * @param string $head
   *
   * @return array
   */
  protected function multibulk($head)
  {
    if (($count = substr($head, 1)) == '-1') {
      return null;
    }

    $response = array();

    // Iterate through each bulk response in the multi-bulk and parse it out.
    for ($i = 0; $i < $count; $i++) {
      $response[] = $this->parse(trim(fgets($this->connection, 512)));
    }

    return $response;
  }

  /**
   * Dynamically make calls to the Redis database.
   *
   * @param string $method
   * @param array $parameters
   *
   * @return mixed
   */
  public function __call($method, $parameters)
  {
    return $this->run($method, $parameters);
  }

  /**
   * Dynamically pass static method calls to the Redis instance.
   *
   * @param $method
   * @param $parameters
   *
   * @return mixed
   */
  public static function __callStatic($method, $parameters)
  {
    return static::database()->run($method, $parameters);
  }

  /**
   * Close the connection to the Redis database.
   *
   * @return void
   */
  public function __destruct()
  {
    if ($this->connection) {
      fclose($this->connection);
    }
  }
}
