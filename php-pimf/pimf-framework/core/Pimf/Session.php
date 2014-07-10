<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Session\Payload;
use Pimf\Session\Storages as Storage;

/**
 * Using the session
 *
 * <code>
 *
 *    // Retrieve the session instance and get an item
 *    Session::instance()->get('name');
 *
 *    // Retrieve the session instance and place an item in the session
 *    Session::instance()->put('name', 'Robin');
 *
 *    // Retrieve a value from the session
 *    $value = Session::get('name');
 *
 *    // Write a value to the session storage
 *    $value = Session::put('name', 'Robin');
 *
 *    // Equivalent statement using the "instance" method
 *    $value = Session::instance()->put('name', 'Robin');
 *
 * </code>
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 * @method static save()
 */
class Session
{
  /**
   * The session singleton instance for the request.
   *
   * @var Payload
   */
  public static $instance;

  /**
   * The string name of the CSRF token stored in the session.
   *
   * @var string
   */
  const CSRF = 'csrf_token';

  /**
   * Create the session payload and load the session.
   *
   * @return void
   */
  public static function load()
  {
    $conf = Registry::get('conf');

    static::start($conf['session']['storage']);

    static::$instance->load(Cookie::get($conf['session']['cookie']));
  }

  /**
   * Create the session payload instance for the request.
   *
   * @param string $storage
   *
   * @return void
   */
  public static function start($storage)
  {
    static::$instance = new Payload(static::factory($storage));
  }

  /**
   * Create a new session storage instance.
   *
   * @param string $storage
   *
   * @return Storage\Storage
   * @throws \RuntimeException
   */
  public static function factory($storage)
  {
    $conf = Registry::get('conf');

    switch ($storage) {
      case 'apc':
        return new Storage\Apc(Cache::storage('apc'));

      case 'cookie':
        return new Storage\Cookie();

      case 'file':
        return new Storage\File($conf['session']['storage_path']);

      case 'pdo':
        return new Storage\Pdo(Pdo\Factory::get($conf['session']['database']));

      case 'memcached':
        return new Storage\Memcached(Cache::storage('memcached'));

      case 'memory':
        return new Storage\Memory();

      case 'redis':
        return new Storage\Redis(Cache::storage('redis'));

      case 'dba':
        return new Storage\Dba(Cache::storage('dba'));

      default:
        throw new \RuntimeException("Session storage [$storage] is not supported.");
    }
  }

  /**
   * Retrieve the active session payload instance for the request.
   *
   * @return Payload
   * @throws \RuntimeException
   */
  public static function instance()
  {
    if (static::started()) {
      return static::$instance;
    }

    throw new \RuntimeException("A storage must be set before using the session.");
  }

  /**
   * Determine if session handling has been started for the request.
   *
   * @return bool
   */
  public static function started()
  {
    return (static::$instance !== null);
  }

  /**
   * Magic Method for calling the methods on the session singleton instance.
   *
   * @param $method
   * @param $parameters
   *
   * @return mixed
   */
  public static function __callStatic($method, $parameters)
  {
    return call_user_func_array(
      array(static::instance(), $method), $parameters
    );
  }
}
