<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Session\Storages;

use Pimf\Contracts\Cleanable;

/**
 * @package Session_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class File extends Storage implements Cleanable
{
  /**
   * The path to which the session files should be written.
   *
   * @var string
   */
  private $path;

  /**
   * @param string $path
   */
  public function __construct($path)
  {
    $this->path = (string)$path;
  }

  /**
   * Load a session from storage by a given ID.
   *
   * @param string $key
   *
   * @return array|mixed
   */
  public function load($key)
  {
    if (file_exists($path = $this->path . $key)) {
      return unserialize(file_get_contents($path));
    }
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
    file_put_contents($this->path . $session['id'], serialize($session), LOCK_EX);
  }

  /**
   * @param string $key
   */
  public function delete($key)
  {
    if (file_exists($this->path . $key)) {
      @unlink($this->path . $key);
    }
  }

  /**
   * Delete all expired sessions from persistent storage.
   *
   * @param int $expiration
   *
   * @return mixed|void
   */
  public function clean($expiration)
  {
    $files = glob($this->path . '*');

    if ($files === false) {
      return;
    }

    foreach ($files as $file) {
      if (filetype($file) == 'file' && filemtime($file) < $expiration) {
        @unlink($file);
      }
    }
  }
}
