<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */
namespace Pimf;

use \Pimf\Util\Header, Pimf\Util\Json as UtilJson;

/**
 * Provides a simple interface around the HTTP an HTTPCache-friendly response generating.
 * Use this class to build and the current HTTP response before it is returned to the client.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Response
{
  /**
   * The request method send by the client-browser.
   *
   * @var string
   */
  protected $method = null;

  /**
   * If the response attempts to send any cached headers.
   *
   * @var bool
   */
  protected static $cached = false;

  /**
   * Type of the data will be send to the client-browser.
   *
   * @var string
   */
  protected static $typed = null;

  /**
   * @param string $requestMethod
   *
   * @throws \RuntimeException
   */
  public function __construct($requestMethod)
  {
    $this->method = '' . strtoupper($requestMethod);

    // it is PIMF framework restriction
    if (!in_array($this->method, array('POST', 'GET', null))) {
      throw new \RuntimeException('unsupported request-method given');
    }

    Header::clear();
  }

  /**
   * @return string
   */
  public function getMethod()
  {
    return $this->method;
  }

  public function asJSON()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asJSON();

    return $this;
  }

  public function asHTML()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asTextHTML();

    return $this;
  }

  public function asPDF()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asPDF();

    return $this;
  }

  public function asCSV()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asCSV();

    return $this;
  }

  public function asTEXT()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asTextPlain();

    return $this;
  }

  public function asZIP()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asZIP();

    return $this;
  }

  public function asXZIP()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asXZIP();

    return $this;
  }

  public function asMSWord()
  {
    $this->preventMultipleTypes();
    self::$typed = __FUNCTION__;
    Header::asMSWord();

    return $this;
  }

  /**
   * Sends a download dialog to the browser.
   *
   * @param string $stream Can be a file-path or a string.
   * @param string $name   Name of the stream/file should be shown.
   */
  public function sendStream($stream, $name)
  {
    Header::clear();
    Header::sendDownloadDialog($stream, $name);
  }

  /**
   * @param mixed $data
   * @param bool  $exit
   */
  public function send($data, $exit = true)
  {
    $body = $data;

    if (self::$typed === 'asJSON') {
      $body = UtilJson::encode($data);
    } elseif ($data instanceof \Pimf\View) {
      $body = $data->render();
    }

    echo '' . $body;
    if ($exit) {
      exit(0);
    }
  }


  /**
   * If instead you have a page that has personalization on it
   * (say, for example, the splash page contains local news as well),
   * you can set a copy to be cached only by the browser.
   *
   * @param int $seconds Interval in seconds
   *
   * @return $this
   */
  public function cacheBrowser($seconds)
  {
    self::preventMultipleCaching();
    self::$cached = true;
    Header::cacheBrowser($seconds);

    return $this;
  }

  /**
   * If you want to try as hard as possible to keep a page from being cached anywhere.
   *
   * @return $this
   */
  public function cacheNone()
  {
    self::preventMultipleCaching();
    self::$cached = true;
    Header::cacheNone();

    return $this;
  }

  /**
   * If you want to allow a page to be cached by shared proxies for one minute.
   *
   * @param int $seconds Interval in seconds
   *
   * @return $this
   */
  public function cacheNoValidate($seconds = 60)
  {
    self::preventMultipleCaching();
    self::$cached = true;
    Header::cacheNoValidate($seconds);

    return $this;
  }

  /**
   * Handles setting pages that are always to be revalidated for freshness by any cache.
   *
   * @param int $last_modified Timestamp in seconds
   *
   * @return $this
   */
  public function exitIfNotModifiedSince($last_modified)
  {
    self::preventMultipleCaching();
    self::$cached = true;
    Header::exitIfNotModifiedSince($last_modified);

    return $this;
  }

  /**
   * @throws \RuntimeException
   */
  private function preventMultipleTypes()
  {
    if (!is_empty(self::$typed)) {
      Header::clear();
      throw new \RuntimeException('only one HTTP content-type can be sent!');
    }
  }

  /**
   * @throws \RuntimeException
   */
  private function preventMultipleCaching()
  {
    if ($this->method != 'GET') {
      Header::clear();
      throw new \RuntimeException('HTTP cache headers can only take effect if request was sent via GET method!');
    }

    if (self::$cached === true) {
      Header::clear();
      throw new \RuntimeException('only one HTTP cache-control can be sent!');
    }
  }
}
