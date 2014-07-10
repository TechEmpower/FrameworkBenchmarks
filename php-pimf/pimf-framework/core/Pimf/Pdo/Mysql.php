<?php
/**
 * Database
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Pdo;

/**
 * Connection management to MySQL.
 *
 * @package Database
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Mysql extends Connector
{
  /**
   * @param array $config
   *
   * @return \Pimf\Database
   */
  public function connect(array $config)
  {
    $dsn = "mysql:host={$config['host']};dbname={$config['database']}";

    if (isset($config['port'])) {
      $dsn .= ";port={$config['port']}";
    }

    if (isset($config['unix_socket'])) {
      $dsn .= ";unix_socket={$config['unix_socket']}";
    }

    $connection = new \Pimf\Database($dsn, $config['username'], $config['password'], $this->options($config));

    // set to UTF-8 which should be fine for most scenarios.
    if (isset($config['charset'])) {
      $connection->prepare("SET NAMES '{$config['charset']}'")->execute();
    }

    return $connection;
  }
}
