<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Json
{
  /**
   * Returns the JSON representation of a value.
   *
   * @param mixed $data
   *
   * @return string
   */
  public static function encode($data)
  {
    $json = json_encode($data);

    self::handleError(json_last_error());

    return $json;
  }

  /**
   * Decodes a JSON string.
   *
   * @param string  $jsonString
   * @param boolean $assoc If should be converted into associative array/s.
   *
   * @return mixed
   */
  public static function decode($jsonString, $assoc = false)
  {
    $json = json_decode($jsonString, $assoc);

    self::handleError(json_last_error());

    return $json;
  }

  /**
   * @param int $status
   *
   * @throws \RuntimeException
   */
  protected static function handleError($status)
  {
    $msg = '';

    switch ($status) {
      case JSON_ERROR_DEPTH:
        $msg = 'Maximum stack depth exceeded';
        break;
      case JSON_ERROR_STATE_MISMATCH:
        $msg = 'Underflow or the modes mismatch';
        break;
      case JSON_ERROR_CTRL_CHAR:
        $msg = 'Unexpected control character found';
        break;
      case JSON_ERROR_SYNTAX:
        $msg = 'Syntax error, malformed JSON';
        break;
      case 5: //alias for JSON_ERROR_UTF8 due to Availability PHP 5.3.3
        $msg = 'Malformed UTF-8 characters, possibly incorrectly encoded';
        break;
    }

    if ($msg !== '') {
      throw new \RuntimeException($msg);
    }
  }
}
