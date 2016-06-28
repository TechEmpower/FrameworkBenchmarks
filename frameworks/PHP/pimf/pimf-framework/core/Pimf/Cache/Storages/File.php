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
class File extends Storage
{
  /**
   * The path to which the cache files should be written.
   *
   * @var string
   */
  protected $path;

  /**
   * @param $path
   */
  public function __construct($path)
  {
    $this->path = $path;
  }

  /**
   * @param string $key
   *
   * @return bool|mixed|null
   */
  protected function retrieve($key)
  {
    if (!file_exists($this->path . $key)) {
      return null;
    }

    // compare the timestamp to the current time when we read the file.
    if (time() >= substr($cache = file_get_contents($this->path . $key), 0, 10)) {
      return $this->forget($key);
    }

    return unserialize(substr($cache, 10));
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
   * @return int|void
   */
  public function put($key, $value, $minutes)
  {
    if ((int)$minutes <= 0) {
      return null;
    }

    $value = $this->expiration($minutes) . serialize($value);

    return file_put_contents($this->path . $key, $value, LOCK_EX);
  }

  /**
   * Write an item to the cache for five years.
   *
   * @param $key
   * @param $value
   *
   * @return int|void
   */
  public function forever($key, $value)
  {
    return $this->put($key, $value, 2628000);
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
    if (file_exists($this->path . $key)) {

      @unlink($this->path . $key);

      clearstatcache();

      return true;
    }

    return false;
  }
}
