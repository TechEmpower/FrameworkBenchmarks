<?php
/**
 * Database
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Pdo;

/**
 * Abstract class for connections and connection management.
 *
 * @package Database
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class Connector
{
  /**
   * The PDO connection options.
   *
   * @var array
   */
  protected $options = array(
      \PDO::ATTR_ERRMODE           => \PDO::ERRMODE_EXCEPTION,
      \PDO::ATTR_ORACLE_NULLS      => \PDO::NULL_NATURAL,
      \PDO::ATTR_STRINGIFY_FETCHES => false,
      \PDO::ATTR_EMULATE_PREPARES  => false,
    );

  /**
   * Establish a PDO database connection.
   *
   * @param array $config
   *
   * @return \PDO
   */
  abstract public function connect(array $config);

  /**
   * Get the PDO connection options for the configuration.
   * Developer specified options will override the default connection options.
   *
   * @param array $config
   *
   * @return array
   */
  protected function options($config)
  {
    $options = (isset($config['options'])) ? $config['options'] : array();

    return $this->options + (array)$options;
  }
}
