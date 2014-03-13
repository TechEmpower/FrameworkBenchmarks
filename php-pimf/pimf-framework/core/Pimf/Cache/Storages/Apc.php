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
class Apc extends Storage
{
  /**
   * The cache key from the cache configuration file.
   *
   * @var string
   */
  protected $key;

  /**
   * Is APCu is supported.
   *
   * @var bool
   */
  protected $apcu = false;

  /**
   * Create a new APC cache storage instance.
   *
   * @param string $key
   */
  public function __construct($key)
  {
    $this->key  = (string)$key;
    $this->apcu = function_exists('apcu_fetch');
  }

  /**
   * Retrieve an item from the cache storage.
   *
   * @param string $key
   *
   * @return mixed
   */
  protected function retrieve($key)
  {
    return $this->apcu ? apcu_fetch($this->key . $key) : apc_fetch($this->key . $key);
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
   * @return bool
   */
  public function put($key, $value, $minutes)
  {
    return $this->apcu
      ? apcu_store('' . $this->key . $key, $value, (int)$minutes * 60)
      : apc_store('' . $this->key . $key, $value, (int)$minutes * 60
      );
  }

  /**
   * Write an item to the cache that lasts forever.
   *
   * @param  string $key
   * @param  mixed  $value
   *
   * @return boolean|null
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
   * @return bool
   */
  public function forget($key)
  {
    return $this->apcu ? apcu_delete($key) : apc_delete($key);
  }

  /**
   * Remove all items from the cache.
   *
   * @return void
   */
  public function flush()
  {
    $this->apcu ? apcu_clear_cache() : apc_clear_cache('user');
  }
}
