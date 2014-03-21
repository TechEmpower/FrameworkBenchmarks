<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Session\Storages;

use Pimf\Cookie as Crumb;

/**
 * @package Session_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Cookie extends Storage
{
  /**
   * The name of the cookie used to store the session payload.
   *
   * @var string
   */
  const PAYLOAD = 'session_payload';

  /**
   * Load a session from storage by a given ID.
   *
   * @param  string $key
   *
   * @return array|null
   */
  public function load($key)
  {
    if (Crumb::has(static::PAYLOAD)) {
      return unserialize(base64_decode(Crumb::get(static::PAYLOAD)));
    }

    return null;
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
    Crumb::put(static::PAYLOAD, base64_encode(serialize($session)), $config['lifetime'], $config['path'], $config['domain']);
  }

  /**
   * @param string $key
   */
  public function delete($key)
  {
    Crumb::forget(static::PAYLOAD);
  }
}
