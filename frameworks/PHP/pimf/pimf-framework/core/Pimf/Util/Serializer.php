<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * Due to PHP Bug #39736 - serialize() consumes insane amount of RAM.
 *
 * Now we can put objects, strings, integers or arrays. Even instances of SimpleXMLElement can be put too!
 *
 * @package Util
 * @link    https://bugs.php.net/bug.php?id=39736
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Serializer
{
  /**
   * Serialize things.
   *
   * @param mixed $object Item you want - string, array, integer, object
   *
   * @return string Containing a byte-stream representation.
   */
  public static function serialize($object)
  {
    $masked = false;

    if (false === is_object($object)) {
      $object = self::mask($object);
      $masked = true;
    }

    $capsule         = new \stdClass();
    $capsule->type   = get_class($object);
    $capsule->object = $object;
    $capsule->fake   = $masked;

    if ($object instanceof \SimpleXMLElement) {
      $capsule->object = $object->asXml();
    }

    return '' . self::serializeNative($capsule);
  }

  /**
   * Unserialize things.
   *
   * @param string $object Serialized object.
   *
   * @return mixed
   */
  public static function unserialize($object)
  {
    $capsule = self::unserializeNative($object);

    if (true === $capsule->fake) {
      $capsule->object = self::unmask($capsule->object);
    }

    if ($capsule->type == 'SimpleXMLElement') {
      $capsule->object = simplexml_load_string($capsule->object);
    }

    return $capsule->object;
  }

  /**
   * @param \stdClass $value Item value.
   *
   * @throws \RuntimeException If error during serialize.
   * @return string
   */
  public static function serializeNative($value)
  {
    $ret = (extension_loaded('igbinary') && function_exists('igbinary_serialize')) ? @igbinary_serialize($value) : @serialize($value);

    self::bombIf($ret);

    return $ret;
  }

  /**
   * @param string $serialized The serialized item-string.
   *
   * @throws \RuntimeException If error during unserialize.
   * @return mixed
   */
  public static function unserializeNative($serialized)
  {
    $ret = (extension_loaded('igbinary') && function_exists('igbinary_unserialize'))
      ? @igbinary_unserialize($serialized)
      : @unserialize(
        $serialized
      );

    self::bombIf($ret);

    return $ret;
  }

  /**
   * @param mixed $item Item
   *
   * @return \stdClass
   */
  private static function mask($item)
  {
    return (object)$item;
  }

  /**
   * @param mixed $item Item
   *
   * @return array
   */
  private static function unmask($item)
  {
    if (isset($item->scalar)) {
      return $item->scalar;
    }

    return (array)$item;
  }

  /**
   * @param boolean $valid
   *
   * @throws \RuntimeException
   */
  private static function bombIf($valid)
  {
    if ($valid === false) {
      $err = error_get_last();
      throw new \RuntimeException($err['message']);
    }
  }
}
