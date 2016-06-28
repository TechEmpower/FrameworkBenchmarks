<?php
/**
 * Database
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Pdo;

/**
 * Creates a PDO connection from the farm of connectors.
 *
 * @package Database
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Factory
{
  /**
   * @param array $config
   *
   * @return \Pimf\Database
   * @throws \RuntimeException If no driver specified or no PDO installed.
   * @throws \UnexpectedValueException
   */
  public static function get(array $config)
  {
    if (!isset($config['driver']) or !$config['driver']) {
      throw new \RuntimeException('no driver specified');
    }

    $driver = strtolower($config['driver']);

    if (!in_array($driver, array('sqlite', 'mysql', 'sqlserver', 'postgre'), true)) {
      throw new \UnexpectedValueException('PDO driver "' . $driver . '" not supported by PIMF');
    }

    if (!extension_loaded('pdo') or !extension_loaded('pdo_' . $driver)) {
      throw new \RuntimeException('Please navigate to "http://php.net/manual/pdo.installation.php" '
        . ' to find out how to install "PDO" with "pdo_' . $driver . '" on your system!');
    }

    $driver = '\Pimf\Pdo\\' . ucfirst($driver);

    $pdo = new $driver();

    return $pdo->connect($config);
  }
}
