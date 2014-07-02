<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Session;

use Pimf\Registry;
use Pimf\Session;
use Pimf\Util\String;
use Pimf\Cookie;

/**
 * Using the session payload
 *
 * <code>
 *
 *    // Get an item from the session
 *    $name = Session::get('name');
 *
 *    // Return a default value if the item doesn't exist
 *    $name = Session::get('name', 'Robin');
 *
 *    // Write an item to the session payload
 *    Session::put('name', 'Robin');
 *
 *    // Write an item to the session payload's flash data
 *    Session::flash('name', 'Robin');
 *
 *    // Keep the "name" item from expiring from the flash data
 *    Session::keep('name');
 *
 *    // Keep the "name" and "email" items from expiring from the flash data
 *    Session::keep(array('name', 'email'));
 *
 * </code>
 *
 * @package Session
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Payload
{
  /**
   * The session array that is stored by the storage.
   *
   * @var array
   */
  public $session;

  /**
   * The session storage used to retrieve and store the session payload.
   *
   * @var \Pimf\Session\Storages\Storage
   */
  public $storage;

  /**
   * Indicates if the session already exists in storage.
   *
   * @var bool
   */
  public $exists = true;

  /**
   * @param Storages\Storage $storage
   */
  public function __construct(\Pimf\Session\Storages\Storage $storage)
  {
    $this->storage = $storage;
  }

  /**
   * Load the session for the current request.
   *
   * @param null|string $key
   */
  public function load($key)
  {
    if ($key !== null) {
      $this->session = $this->storage->load($key);
    }

    // If the session doesn't exist or is invalid.
    if (is_null($this->session) or static::expired($this->session)) {
      $this->exists  = false;
      $this->session = $this->storage->fresh();
    }

    // A CSRF token is stored in every session to protect
    // the application from cross-site request
    if (!$this->has(Session::CSRF)) {
      $this->put(Session::CSRF, String::random(40));
    }
  }

  /**
   * Determine if the session payload instance is valid.
   *
   * @param array $session
   *
   * @return bool
   */
  protected static function expired($session)
  {
    $conf = Registry::get('conf');

    if (array_key_exists('last_activity', $session)) {
      return (time() - $session['last_activity']) > ($conf['session']['lifetime'] * 60);
    }

    return false;
  }

  /**
   * Determine if the session or flash data contains an item.
   *
   * @param string $key
   *
   * @return bool
   */
  public function has($key)
  {
    return ($this->get($key) !== null);
  }

  /**
   * Get an item from the session.
   *
   * @param string $key
   * @param null   $default
   *
   * @return mixed|null
   */
  public function get($key, $default = null)
  {
    $session = $this->session['data'];

    // check first for the item in the general session data.
    if (null !== ($value = $this->isIn($key, $session))) {
      return $value;
    }

    // or find it in the new session data.
    if (null !== ($value = $this->isIn($key, $session[':new:']))) {
      return $value;
    }

    // or finally return the default value.
    if (null !== ($value = $this->isIn($key, $session[':old:']))) {
      return $value;
    }

    return $default;
  }

  /**
   * Checks if key is in session.
   *
   * @param string $key
   * @param array  $session
   *
   * @return mixed|null
   */
  protected function isIn($key, array $session)
  {
    if (array_key_exists($key, $session) and $session[$key] !== null) {
      return $session[$key];
    }

    return null;
  }

  /**
   * Write an item to the session.
   *
   * @param $key
   * @param string $value
   */
  public function put($key, $value)
  {
    $this->session['data'][$key] = $value;
  }

  /**
   * Write an item to the session flash data.
   *
   * @param $key
   * @param $value
   */
  public function flash($key, $value)
  {
    $this->session['data'][':new:'][$key] = $value;
  }

  /**
   * Keep all of the session flash data from expiring after the request.
   *
   * @return void
   */
  public function reflash()
  {
    $this->session['data'][':new:'] = array_merge(
      $this->session['data'][':new:'], $this->session['data'][':old:']
    );
  }

  /**
   * Keep a session flash item from expiring at the end of the request.
   *
   * @param $keys
   */
  public function keep($keys)
  {
    foreach ((array)$keys as $key) {
      $this->flash($key, $this->get($key));
    }
  }

  /**
   * Remove an item from the session data.
   *
   * @param $key
   */
  public function forget($key)
  {
    unset($this->session['data'][$key]);
  }

  /**
   * Remove all of the items from the session (CSRF token will not be removed).
   */
  public function flush()
  {
    $this->session['data'] = array(Session::CSRF => $this->token(), ':new:' => array(), ':old:' => array());
  }

  /**
   * Assign a new, random ID to the session.
   */
  public function regenerate()
  {
    $this->session['id'] = $this->storage->id();
    $this->exists        = false;
  }

  /**
   * Get the CSRF token that is stored in the session data.
   *
   * @return string
   */
  public function token()
  {
    return $this->get(Session::CSRF);
  }

  /**
   * Get the last activity for the session.
   *
   * @return int
   */
  public function activity()
  {
    return $this->session['last_activity'];
  }

  /**
   * Store the session payload in storage.
   * This method will be called automatically at the end of the request.
   *
   * @return void
   */
  public function save()
  {
    $this->session['last_activity'] = time();

    // age it that should expire at the end of the user's next request.
    $this->age();

    $conf        = Registry::get('conf');
    $sessionConf = $conf['session'];

    // save data into the specialized storage.
    $this->storage->save($this->session, $sessionConf, $this->exists);

    // determine the owner of the session
    // on the user's subsequent requests to the application.
    $this->cookie($sessionConf);

    // calculate and run garbage collection cleaning.
    $cleaning = $sessionConf['garbage_collection'];

    if (mt_rand(1, $cleaning[1]) <= $cleaning[0]) {
      $this->clean();
    }
  }

  /**
   * Clean up expired sessions.
   *
   * @return void
   */
  public function clean()
  {
    if ($this->storage instanceof \Pimf\Contracts\Cleanable) {
      $conf = Registry::get('conf');
      $this->storage->clean(time() - ($conf['session']['lifetime'] * 60));
    }
  }

  /**
   * Age the session flash data.
   *
   * @return void
   */
  protected function age()
  {
    $this->session['data'][':old:'] = $this->session['data'][':new:'];
    $this->session['data'][':new:'] = array();
  }

  /**
   * Send the session ID cookie to the browser.
   *
   * @param  array $config
   *
   * @return void
   */
  protected function cookie($config)
  {
    $minutes = (!$config['expire_on_close']) ? $config['lifetime'] : 0;

    Cookie::put($config['cookie'], $this->session['id'], $minutes, $config['path'], $config['domain'], $config['secure']);
  }
}
