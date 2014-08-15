<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

use Pimf\Registry;
use Pimf\Sapi;

/**
 * Manages a raw HTTP header sending.
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Header extends Header\ContentType
{
  /**
   * Removes previously set headers.
   */
  public static function clear()
  {
    if (!headers_sent() && error_get_last() == null) {
      header_remove();
    }
  }

  /**
   * @param string  $url
   * @param boolean $exit
   */
  public static function toLocation($url, $exit = true)
  {
    header('Location: ' . $url);
    if ($exit) {
      exit(1);
    }
  }

  /**
   * @param int     $code
   * @param string  $status
   * @param boolean $exit
   */
  protected static function view($code, $status, $exit = true)
  {
    if (Sapi::isCli()) {
      echo $status . PHP_EOL;
      if ($exit) {
        exit;
      }
    }

    self::send($code, $status);

    $conf    = Registry::get('conf');
    $appTpl  = str_replace('/', DS, BASE_PATH . 'app/' . $conf['app']['name'] . '/_error/' . $code . '.php');
    $coreTpl = str_replace('/', DS, BASE_PATH . 'pimf-framework/core/Pimf/_error/' . $code . '.php');
    $coreTpl = str_replace(DS.'pimf-framework'.DS.'pimf-framework'.DS, DS.'pimf-framework'.DS, $coreTpl);

    if (file_exists($appTpl) && is_readable($appTpl)) {
      include $appTpl;
      if ($exit) {
        exit(1);
      }
    }

    include $coreTpl;
    if ($exit) {
      exit(1);
    }
  }

  /**
   * @param string  $msg
   * @param boolean $exit
   */
  public static function sendInternalServerError($msg = '', $exit = true)
  {
    self::view(500, $msg, $exit);
  }

  /**
   * @param string  $msg
   * @param boolean $exit
   */
  public static function sendNotFound($msg = '', $exit = true)
  {
    self::view(404, $msg, $exit);
  }

  /**
   * Handles setting pages that are always to be revalidated for freshness by any cache.
   *
   * @param int $last_modified Timestamp in seconds
   */
  public static function exitIfNotModifiedSince($last_modified)
  {
    if (self::isModified($last_modified)) {
      self::sendNotModified();
      exit(0);
    }

    $last_modified = gmdate('D, d M Y H:i:s', $last_modified) . ' GMT';
    header("Cache-Control: must-revalidate");
    header("Last Modified: $last_modified");
  }

  /**
   * Actual HTTP caching validation.
   *
   * @param int    $mtime In seconds
   * @param string $etag
   *
   * @return bool
   */
  public static function isModified($mtime, $etag = '')
  {
    $env = Registry::get('env');

    $modified_since = strtotime(preg_replace('/;.*$/', '', $env->HTTP_IF_MODIFIED_SINCE));

    return !($modified_since >= $mtime || $env->HTTP_IF_NONE_MATCH == $etag);
  }

  /**
   * If you want to allow a page to be cached by shared proxies for one minute.
   *
   * @param int $seconds Interval in seconds
   */
  public static function cacheNoValidate($seconds = 60)
  {
    $now    = time();
    $lmtime = gmdate('D, d M Y H:i:s', $now) . ' GMT';
    $extime = gmdate('D, d M Y H:i:s', $now + $seconds) . 'GMT';
    // backwards compatibility for HTTP/1.0 clients
    header("Last Modified: $lmtime");
    header("Expires: $extime");
    // HTTP/1.1 support
    header("Cache-Control: public,max-age=$seconds");
  }

  /**
   * If instead you have a page that has personalization on it
   * (say, for example, the splash page contains local news as well),
   * you can set a copy to be cached only by the browser.
   *
   * @param int $seconds Interval in seconds
   */
  public static function cacheBrowser($seconds = 60)
  {
    $now    = time();
    $lmtime = gmdate('D, d M Y H:i:s', $now) . ' GMT';
    $extime = gmdate('D, d M Y H:i:s', $now + $seconds) . ' GMT';
    // backwards compatibility for HTTP/1.0 clients
    header("Last Modified: $lmtime");
    header("Expires: $extime");
    // HTTP/1.1 support
    header("Cache-Control: private,max-age=$seconds,s-maxage=0");
  }


  /**
   * If you want to try as hard as possible to keep a page from being cached anywhere.
   */
  public static function cacheNone()
  {
    // backwards compatibility for HTTP/1.0 clients
    header("Expires: 0");
    header("Pragma: no-cache");
    // HTTP/1.1 support
    header("Cache-Control: no-cache,no-store,max-age=0,s-maxage=0,must-revalidate");
  }

  /**
   * Sends file as download-header through any firewall to the browsers like >=IE6 >=FF3.6, Safari, Chrome, Opera.
   *
   * @link http://reeg.junetz.de/DSP/node16.html
   * @link http://www.php.net/manual/de/function.header.php#88038
   *
   * @param string $fileOrString
   * @param string $fileName
   * @param boolean $exit Optional for testing
   */
  public static function sendDownloadDialog($fileOrString, $fileName, $exit = true)
  {
    $disposition = (false !== strpos(Registry::get('env')->getUserAgent(), 'MSIE 5.5')) ? '' : 'attachment; ';

    header("Pragma: public");
    header("Expires: 0");
    header("Cache-Control: must-revalidate, post-check=0, pre-check=0");
    header("Cache-Control: private", false);
    header("Content-Disposition: " . $disposition . "filename=" . $fileName . ";");

    if (is_file($fileOrString)) {
      readfile($fileOrString);
    } else {
      echo $fileOrString;
    }

    if ($exit) {
      exit(0);
    }
  }
}
