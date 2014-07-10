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
class Pdo extends Storage
{
  /**
   * The cache key from the cache configuration file.
   *
   * @var string
   */
  protected $key;

  /**
   * @var \Pimf\Database
   */
  protected $pdo;

  /**
   * Create a new database cache storage instance.
   *
   * @param \Pimf\Database $pdo
   * @param string $key
   */
  public function __construct(\Pimf\Database $pdo, $key)
  {
    $this->pdo = $pdo;
    $this->key = (string)$key;
  }

  /**
   * Retrieve an item from the cache storage.
   *
   * @param string $key
   *
   * @return mixed|void
   */
  protected function retrieve($key)
  {
    $sth = $this->pdo->prepare(
      'SELECT * FROM pimf_cache WHERE key = :key'
    );

    $sth->bindValue(':key', $this->key . $key);
    $sth->execute();

    $cache = $sth->fetchObject();

    if ($cache instanceof \stdClass) {

      if (time() >= $cache->expiration) {
        return $this->forget($key);
      }

      return unserialize($cache->value);
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
   * @param  string $key
   * @param  mixed  $value
   * @param  int    $minutes
   *
   * @return bool
   */
  public function put($key, $value, $minutes)
  {
    $key        = $this->key . $key;
    $value      = serialize($value);
    $expiration = $this->expiration($minutes);

    try {
      $sth = $this->pdo->prepare(
        "INSERT INTO pimf_cache (key, value, expiration) VALUES (:key, :value, :expiration)"
      );
    } catch (\Exception $exception) {
      $sth = $this->pdo->prepare(
        "UPDATE pimf_cache SET value = :value, expiration = :expiration WHERE key = :key"
      );
    }

    $sth->bindValue(':key', $key);
    $sth->bindValue(':value', $value);
    $sth->bindValue(':expiration', $expiration);

    return $sth->execute();
  }

  /**
   * Write an item to the cache for five years.
   *
   * @param $key
   * @param $value
   *
   * @return bool
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
   * @return boolean
   */
  public function forget($key)
  {
    $sth = $this->pdo->prepare(
      "DELETE FROM pimf_cache WHERE key = :key"
    );

    $sth->bindValue(':key', $this->key . $key);

    return $sth->execute();
  }
}
