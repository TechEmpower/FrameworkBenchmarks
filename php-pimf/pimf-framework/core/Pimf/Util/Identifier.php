<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * Identifier util for unified resource generation.
 *
 * <code>
 * $identifier = new class Identifier(1, '23', 123, 'ZZ-TOP', 'Some_Class_name');
 *
 * print $identifier; // --> '1_23_123_zz_top_some_class_name'
 *
 * $identifier->setDelimiter('/');
 *
 * print $identifier->generate(); // --> '1/23/123/zz/top/some/class/name'
 * </code>
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Identifier
{
  /**
   * @var string
   */
  protected static $delimiter = '_';

  /**
   * @var array
   */
  protected $args = array();

  /**
   * Create a new Cache Identifier based on the given parameters.
   * Integer and string but not array and objects.
   *
   * @throws \BadMethodCallException If no identifiers received.
   */
  public function __construct()
  {
    $args = func_get_args();

    if (!count($args) || !implode('', $args)) {
      throw new \BadMethodCallException('No identifiers received');
    }

    $this->args = $args;
  }

  /**
   * Return String representation of this Cache Identifier.
   *
   * @return string
   */
  public function __toString()
  {
    return $this->generate();
  }

  /**
   * @return string
   */
  public function generate()
  {
    return (string)$this->slag();
  }

  /**
   * Slags the identifier.
   *
   * @return string
   */
  protected function slag()
  {
    $ident = str_replace('-', '_', implode(self::getDelimiter(), $this->args));
    $ident = str_replace('_', self::getDelimiter(), $ident);
    $ident = trim($ident);
    $ident = str_replace(' ', '', $ident);

    return strip_tags(strtolower($ident));
  }

  /**
   * Set the delimiter used to create a Cache Identifier.
   *
   * @param string $delimiter The delimiter character.
   */
  public function setDelimiter($delimiter)
  {
    self::$delimiter = $delimiter;
  }

  /**
   * Get the delimiter used to create a Cache Identifier.
   *
   * @return string
   */
  public function getDelimiter()
  {
    return self::$delimiter;
  }
}
