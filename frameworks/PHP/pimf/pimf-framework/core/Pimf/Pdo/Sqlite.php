<?php
/**
 * Database
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Pdo;

/**
 * Connection management to SQLite.
 *
 * @package Database
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Sqlite extends Connector
{
  /**
   * @param array $config
   *
   * @return \Pimf\Database
   */
  public function connect(array $config)
  {
    $options = $this->options($config);

    // SQLite provides supported for "in-memory" databases, which exist only for
    // lifetime of the request. These are mainly for tests.
    if ($config['database'] == ':memory:') {
      return new \Pimf\Database('sqlite::memory:', null, null, $options);
    }

    return new \Pimf\Database('sqlite:' . $config['database'], null, null, $options);
  }
}
