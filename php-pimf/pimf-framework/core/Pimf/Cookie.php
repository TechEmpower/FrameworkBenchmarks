<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

/**
 * Using the cookie
 *
 * <code>
 *    // Get the value of the "favorite" cookie
 *    $favorite = Cookie::get('favorite');
 *
 *    // Get the value of a cookie or return a default value
 *    $favorite = Cookie::get('framework', 'Pimf');
 *
 *    // Set the value of the "favorite" cookie
 *    Cookie::put('favorite', 'Pimf');
 *
 *    // Set the value of the "favorite" cookie for twenty minutes
 *    Cookie::put('favorite', 'Pimf', 20);
 *
 *    // Set a cookie that should last one year
 *    Cookie::forever('favorite', 'Blue');
 *
 * </code>
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Cookie
{
  /**
   * How long is forever (in minutes)?
   *
   * @var int
   */
  const FOREVER = 2628000;

  /**
   * The cookies that have been set.
   *
   * @var array
   */
  public static $jar = array();

  /**
   * Determine if a cookie exists.
   *
   * @param  string $name
   *
   * @return bool
   */
  public static function has($name)
  {
    return (static::get($name) !== null);
  }

  /**
   * Get the value of a cookie.
   *
   * @param      $name
   * @param null $default
   *
   * @return null|string
   */
  public static function get($name, $default = null)
  {
    if (isset(static::$jar[$name])) {
      return static::parse(static::$jar[$name]['value']);
    }

    $cookie = Request::$cookieData;

    if (!is_null($value = $cookie->get($name))) {
      return static::parse($value);
    }

    return $default;
  }

  /**
   * Set the value of a cookie.
   *
   * @param        $name
   * @param        $value
   * @param int    $expiration
   * @param string $path
   * @param null   $domain
   * @param bool   $secure
   *
   * @return bool
   * @throws \RuntimeException
   */
  public static function put($name, $value, $expiration = 0, $path = '/', $domain = null, $secure = false)
  {
    if ($expiration !== 0) {
      $expiration = time() + ($expiration * 60);
    }

    $value = static::hash($value) . '+' . $value;

    // If we are attempting to send a secure cookie over the insecure HTTP.
    $conf = Registry::get('conf');

    if ($secure === true and $conf['ssl'] === false) {
      throw new \RuntimeException("Attempting to set secure cookie over HTTP!");
    }

    static::$jar[$name] = compact('name', 'value', 'expiration', 'path', 'domain', 'secure');

    return true;
  }

  /**
   * Set a "permanent" cookie. The cookie will last for one year.
   *
   * @param        $name
   * @param        $value
   * @param string $path
   * @param null   $domain
   * @param bool   $secure
   *
   * @return bool
   */
  public static function forever($name, $value, $path = '/', $domain = null, $secure = false)
  {
    return static::put($name, $value, static::FOREVER, $path, $domain, $secure);
  }

  /**
   * Delete a cookie.
   *
   * @param        string $name
   * @param string $path
   * @param null   $domain
   * @param bool   $secure
   *
   * @return bool
   */
  public static function forget($name, $path = '/', $domain = null, $secure = false)
  {
    return static::put($name, null, -2000, $path, $domain, $secure);
  }

  /**
   * Hash the given cookie value.
   *
   * @param string $value
   *
   * @return string
   */
  public static function hash($value)
  {
    $conf = Registry::get('conf');

    return hash_hmac('sha1', $value, $conf['app']['key']);
  }

  /**
   * Parse a hash fingerprinted cookie value.
   *
   * @param string $value
   *
   * @return string
   */
  protected static function parse($value)
  {
    $segments = explode('+', $value);

    // check if the cookie is invalid.
    if (!(count($segments) >= 2)) {
      return null;
    }

    $value = implode('+', array_slice($segments, 1));

    // check the SHA-1 hash from the cookie.
    if ($segments[0] == static::hash($value)) {
      return $value;
    }

    return null;
  }

  /**
   * Send along with the rest of the HTTP headers.
   */
  public static function send()
  {
    foreach (static::$jar as $cookie) {
      setcookie($cookie['name'], $cookie['value'], $cookie['expiration'], $cookie['path'], $cookie['domain'], $cookie['secure'], true);
    }
  }
}
