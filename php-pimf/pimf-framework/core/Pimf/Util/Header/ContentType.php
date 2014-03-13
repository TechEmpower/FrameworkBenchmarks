<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */
namespace Pimf\Util\Header;

/**
 * Manages a raw HTTP header ContentType sending.
 *
 * @package Util_Header
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class ContentType extends ResponseStatus
{
  public static function contentTypeJson()
  {
    self::type('application/json; charset=utf-8');
  }

  public static function contentTypePdf()
  {
    self::type('application/pdf');
  }

  public static function contentTypeCsv()
  {
    self::type('text/csv');
  }

  public static function contentTypeTextPlain()
  {
    self::type('text/plain');
  }

  public static function contentTypeTextHTML()
  {
    self::type('text/html');
  }

  public static function contentTypeZip()
  {
    self::type('application/zip');
  }

  public static function contentTypeXZip()
  {
    self::type('application/x-zip');
  }

  public static function contentTypeMSWord()
  {
    self::type('application/msword');
  }

  public static function contentTypeOctetStream()
  {
    self::type('application/octet-stream');
  }

  /**
   * @param string $definition
   */
  public static function type($definition)
  {
    header('Content-Type: ' . $definition, true);
  }
}
