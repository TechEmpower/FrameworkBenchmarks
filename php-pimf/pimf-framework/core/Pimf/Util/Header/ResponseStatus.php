<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */
namespace Pimf\Util\Header;

use Pimf\Registry;

/**
 * Manages a raw HTTP header ResponseStatus sending.
 *
 * @package Util_Header
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class ResponseStatus
{
  /**
   * @param int    $code    HTTP response code
   * @param string $status  The header string which will be used to figure out the HTTP status code to send.
   * @param bool   $replace Whether the header should replace a previous similar header.
   */
  public static function send($code, $status, $replace = true)
  {
    header('' . Registry::get('env')->SERVER_PROTOCOL . ' ' . $code . ' ' . $status, $replace, $code);
  }

  public static function sendXFrameDeny()
  {
    header('X-Frame-Options: DENY');
  }

  public static function sendXFrameSameOrigin()
  {
    header('X-Frame-Options: SAMEORIGIN');
  }

  public static function sendContinue()
  {
    self::send(100, 'Continue');
  }

  public static function sendProcessing()
  {
    self::send(102, 'Processing');
  }

  public static function sendOK()
  {
    self::send(200, 'OK');
  }

  public static function sendCreated()
  {
    self::send(201, 'Created');
  }

  public static function sendAccepted()
  {
    self::send(202, 'Accepted');
  }

  public static function sendNoAuthInfo()
  {
    self::send(203, 'Non-Authoritative Information');
  }

  public static function sendNoContent()
  {
    self::send(204, 'No Content');
  }

  public static function sendMovedPermanently()
  {
    self::send(301, 'Moved Permanently');
  }

  public static function sendFound()
  {
    self::send(302, 'Found');
  }

  public function sendNotModified()
  {
    self::send(304, 'Not Modified');
  }

  public static function sendTemporaryRedirect()
  {
    self::send(307, 'Temporary Redirect');
  }

  public static function sendBadRequest()
  {
    self::send(400, 'Bad Request');
  }

  public static function sendUnauthorized()
  {
    self::send(401, 'Unauthorized');
  }

  public static function sendPaymentRequired()
  {
    self::send(402, 'Payment Required');
  }

  public static function sendForbidden()
  {
    self::send(403, 'Forbidden');
  }

  public static function sendMethodNotAllowed()
  {
    self::send(405, 'Method Not Allowed');
  }

  public static function sendNotAcceptable()
  {
    self::send(406, 'Not Acceptable');
  }

  public static function sendProxyAuthRequired()
  {
    self::send(407, 'Proxy Authentication Required');
  }

  public static function sendRequestTimeout()
  {
    self::send(408, 'Request Timeout');
  }

  public static function sendUnsupportedMediaType()
  {
    self::send(415, 'Unsupported Media Type');
  }

  public static function sendLocked()
  {
    self::send(423, 'Locked');
  }

  public static function sendServiceUnavailable()
  {
    self::send(503, 'Service Unavailable');
  }
}
