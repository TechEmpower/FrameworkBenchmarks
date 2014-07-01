<?php
/**
 * Database
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Pdo;

/**
 * Connection management to SQL Server
 *
 * @package Database
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Sqlserver extends Connector
{
  protected $options = array(
      \PDO::ATTR_CASE              => \PDO::CASE_LOWER,
      \PDO::ATTR_ERRMODE           => \PDO::ERRMODE_EXCEPTION,
      \PDO::ATTR_ORACLE_NULLS      => \PDO::NULL_NATURAL,
      \PDO::ATTR_STRINGIFY_FETCHES => false,
    );

  /**
   * @param array $config
   *
   * @return \Pimf\Database
   */
  public function connect(array $config)
  {
    // This connection string format can also be used to connect
    // to Azure SQL Server databases.
    $port = (isset($config['port'])) ? ',' . $config['port'] : '';

    //check for dblib for mac users connecting to mssql
    if (isset($config['dsn_type']) && !empty($config['dsn_type']) and $config['dsn_type'] == 'dblib') {
      $dsn = "dblib:host={$config['host']}{$port};dbname={$config['database']}";
    } else {
      $dsn = "sqlsrv:Server={$config['host']}{$port};Database={$config['database']}";
    }

    return new \Pimf\Database($dsn, $config['username'], $config['password'], $this->options($config));
  }
}
