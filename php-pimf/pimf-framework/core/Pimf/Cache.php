<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Util\String;
use Pimf\Cache\Storages as CS;

/**
 * Cache usage
 *
 * <code>
 *    // Get the default cache storage instance
 *    $storage = Cache::storage();
 *
 *    // Get a specific cache storage instance by name
 *    $storage = Cache::storage('memcached');
 *
 *    // Call the "get" method on the default cache storage
 *    $name = Cache::get('name');
 *
 *    // Call the "put" method on the default cache storage
 *    Cache::put('name', 'Robin', 15);
 * </code>
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Cache
{
  /**
   * All of the active cache storages.
   *
   * @var \Pimf\Cache\Storages\Storage[]
   */
  public static $storages = array();

  /**
   * Get a cache storage instance.
   *
   * @param string $storage
   *
   * @return CS\Apc|CS\Dba|CS\File|CS\Memcached|CS\Memory|CS\Pdo|CS\Redis|CS\WinCache
   */
  public static function storage($storage = 'memory')
  {
    if (!isset(static::$storages[$storage])) {
      static::$storages[$storage] = static::factory($storage);
    }

    return static::$storages[$storage];
  }

  /**
   * Create a new cache storage instance.
   *
   * @param string $storage
   *
   * @return CS\Apc|CS\Dba|CS\File|CS\Memcached|CS\Memory|CS\Pdo|CS\Redis|CS\WinCache
   * @throws \RuntimeException
   */
  protected static function factory($storage)
  {
    $conf = Registry::get('conf');

    switch ($storage) {
      case 'apc':
        return new CS\Apc($conf['cache']['key']);

      case 'file':
        return new CS\File($conf['cache']['storage_path']);

      case 'pdo':
        return new CS\Pdo(Pdo\Factory::get($conf['cache']['database']), $conf['cache']['key']);

      case 'memcached':
        return new CS\Memcached(Memcached::connection(), $conf['cache']['key']);

      case 'memory':
        return new CS\Memory();

      case 'redis':
        return new CS\Redis(Redis::database());

      case 'wincache':
        return new CS\WinCache($conf['cache']['key']);

      case 'dba':
        return new CS\Dba(String::ensureTrailing('/', $conf['cache']['storage_path']) . $conf['cache']['key']);

      default:
        throw new \RuntimeException("Cache storage {$storage} is not supported.");
    }
  }

  /**
   * Magic Method for calling the methods on the default cache storage.
   *
   * @param $method
   * @param $parameters
   *
   * @return mixed
   */
  public static function __callStatic($method, $parameters)
  {
    return call_user_func_array(
      array(static::storage(), $method), $parameters
    );
  }
}
