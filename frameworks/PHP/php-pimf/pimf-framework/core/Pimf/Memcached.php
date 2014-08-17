<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

/**
 * For use please add the following code to the end of the config.core.php file:
 *
 * <code>
 *
 * 'cache' => array(
 *
 *    'storage' => 'memcached',
 *       'servers' => array(
 *           array('host' => '127.0.0.1', 'port' => 11211, 'weight' => 100),
 *        ),
 *      ),
 *  ),
 *
 * </code>
 *
 * Memcached usage:
 *
 * <code>
 *    // Get the Memcache connection and get an item from the cache
 *    $name = Memcached::connection()->get('name');
 *
 *    // Get the Memcache connection and place an item in the cache
 *    Memcached::connection()->set('name', 'Robin');
 *
 *    // Get an item from the Memcache instance
 *    $name = Memcached::get('name');
 *
 *    // Store data on the Memcache server
 *    Memcached::set('name', 'Robin');
 * </code>
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 * @method get($key)
 * @method put($key, $value, $expiration)
 * @method forget($key);
 */
class Memcached
{
  /**
   * @var \Memcached
   */
  protected static $connection;

  /**
   * @return \Memcached
   */
  public static function connection()
  {
    if (static::$connection === null) {
      $conf               = Registry::get('conf');
      static::$connection = static::connect(
        $conf['cache']['servers']
      );
    }

    return static::$connection;
  }

  /**
   * Create a new Memcached connection instance.
   *
   * @param array $servers
   * @param null  $memcache
   *
   * @return \Memcached|null
   * @throws \RuntimeException
   */
  protected static function connect(array $servers, $memcache = null)
  {
    if (!$memcache) {
      $memcache = new \Memcached();
    }

    foreach ($servers as $server) {
      $memcache->addServer($server['host'], $server['port'], $server['weight']);
    }

    if ($memcache->getVersion() === false) {
      throw new \RuntimeException('could not establish memcached connection!');
    }

    return $memcache;
  }

  /**
   * Dynamically pass all other method calls to the Memcache instance.
   *
   * @param $method
   * @param $parameters
   *
   * @return mixed
   */
  public static function __callStatic($method, $parameters)
  {
    return call_user_func_array(array(static::connection(), $method), $parameters);
  }
}
