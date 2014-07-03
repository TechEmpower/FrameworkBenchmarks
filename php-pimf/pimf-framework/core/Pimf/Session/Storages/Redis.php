<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Session\Storages;

/**
 * @package Session_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Redis extends Storage
{
  /**
   * The Redis cache storage instance.
   *
   * @var \Pimf\Cache\Storages\Redis
   */
  protected $redis;

  /**
   * @param \Pimf\Cache\Storages\Redis $redis
   */
  public function __construct(\Pimf\Cache\Storages\Redis $redis)
  {
    $this->redis = $redis;
  }

  /**
   * Load a session from storage by a given ID.
   *
   * @param string $key
   *
   * @return array|mixed|null
   */
  public function load($key)
  {
    return $this->redis->get($key);
  }

  /**
   * Save a given session to storage.
   *
   * @param array $session
   * @param array $config
   * @param bool  $exists
   */
  public function save($session, $config, $exists)
  {
    $this->redis->put($session['id'], $session, $config['lifetime']);
  }

  /**
   * @param string $key
   */
  public function delete($key)
  {
    $this->redis->forget($key);
  }
}
