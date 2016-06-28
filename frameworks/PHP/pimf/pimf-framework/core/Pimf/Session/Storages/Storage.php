<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Session\Storages;

use Pimf\Util\String;

/**
 * @package Session_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class Storage
{
  /**
   * Load a session from storage by a given ID.
   * If no session is found for the id, null will be returned.
   *
   * @param string $key
   *
   * @return array|null
   */
  abstract public function load($key);

  /**
   * Save a given session to storage.
   *
   * @param array $session
   * @param array $config
   * @param bool  $exists
   *
   * @return void
   */
  abstract public function save($session, $config, $exists);

  /**
   * Delete a session from storage by a given ID.
   *
   * @param string $key
   *
   * @return void
   */
  abstract public function delete($key);

  /**
   * Create a fresh session array with a unique ID.
   *
   * @return array
   */
  public function fresh()
  {
    return array('id' => $this->id(), 'data' => array(':new:' => array(), ':old:' => array(),));
  }

  /**
   * Get a new session ID that isn't assigned to any current session.
   *
   * @return string
   */
  public function id()
  {
    // just return any string since the Cookie storage has no idea.
    if ($this instanceof \Pimf\Session\Storages\Cookie) {
      return String::random(40);
    }

    // we'll find an random ID here.
    do {
      $session = $this->load($key = String::random(40));
    } while ($session !== null);

    return $key;
  }
}
