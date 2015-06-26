<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Cache\Storages;

/**
 * @package Cache_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Memcached extends Storage
{
  /**
   * The Memcache instance.
   *
   * @var \Memcached
   */
  public $memcache;

  /**
   * The cache key from the cache configuration file.
   *
   * @var string
   */
  protected $key;

  /**
   * @param \Memcached $memcache
   * @param            $key
   */
  public function __construct(\Memcached $memcache, $key)
  {
    $this->key      = $key;
    $this->memcache = $memcache;
  }

  /**
   * @param string $key
   *
   * @return mixed
   */
  protected function retrieve($key)
  {
    if (($cache = $this->memcache->get($this->key . $key)) !== false) {
      return $cache;
    }
  }

  /**
   * Write an item to the cache for a given number of minutes.
   *
   * <code>
   *    // Put an item in the cache for 15 minutes
   *    Cache::put('name', 'Robin', 15);
   * </code>
   *
   * @param string $key
   * @param mixed  $value
   * @param int    $minutes
   *
   * @return bool|void
   */
  public function put($key, $value, $minutes)
  {
    return $this->memcache->set($this->key . $key, $value, $minutes * 60);
  }

  /**
   * Write an item to the cache that lasts forever.
   *
   * @param $key
   * @param $value
   *
   * @return bool|void
   */
  public function forever($key, $value)
  {
    return $this->put($key, $value, 0);
  }

  /**
   * Delete an item from the cache.
   *
   * @param string $key
   *
   * @return bool|void
   */
  public function forget($key)
  {
    return $this->memcache->delete($this->key . $key);
  }
}
