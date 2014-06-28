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
  public static function asJSON()
  {
    self::type('application/json; charset=utf-8');
  }

  public static function asPDF()
  {
    self::type('application/pdf');
  }

  public static function asCSV()
  {
    self::type('text/csv');
  }

  public static function asTextPlain()
  {
    self::type('text/plain');
  }

  public static function asTextHTML()
  {
    self::type('text/html');
  }

  public static function asZIP()
  {
    self::type('application/zip');
  }

  public static function asXZIP()
  {
    self::type('application/x-zip');
  }

  public static function asMSWord()
  {
    self::type('application/msword');
  }

  public static function asOctetStream()
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
