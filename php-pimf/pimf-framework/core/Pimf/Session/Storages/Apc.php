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
class Apc extends Storage
{

  /**
   * @var \Pimf\Cache\Storages\Apc
   */
  private $apc;

  /**
   * @param \Pimf\Cache\Storages\Apc $apc
   */
  public function __construct(\Pimf\Cache\Storages\Apc $apc)
  {
    $this->apc = $apc;
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
    return $this->apc->get($key);
  }

  /**
   * @param array $session
   * @param array $config
   * @param bool  $exists
   */
  public function save($session, $config, $exists)
  {
    $this->apc->put($session['id'], $session, $config['lifetime']);
  }

  /**
   * @param string $key
   */
  public function delete($key)
  {
    $this->apc->forget($key);
  }

}
