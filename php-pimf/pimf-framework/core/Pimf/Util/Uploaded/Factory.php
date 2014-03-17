<?php
/**
 * Util\Uploaded
 *
 * @copyright Copyright (c) 2010-2013 Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */
namespace Pimf\Util\Uploaded;

use \Pimf\Util\Uploaded;

/**
 * A file uploaded through a form.
 *
 * <code>
 *
 *   // Create an instance using the factory method for more security.
 *   $upload = \Pimf\Util\Uploaded\Factory::get($_FILES);
 *
 *   if ($upload instanceof Uploaded) {
 *     $upload->move('path/to/your/images/dir', $upload->getClientOriginalName());
 *   }
 *
 * </code>
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class Factory
{
  /**
   * @var array
   */
  protected static $fileKeys = array('error', 'name', 'size', 'tmp_name', 'type');

  /**
   * Factory for save instance creation.
   *
   * <code>
   *   // Create an instance using the factory method.
   *   $file = \Pimf\Util\Uploaded\Factory::get($_FILES);
   * </code>
   *
   * @param mixed $file A $_FILES multi-dimensional array of uploaded file information.
   * @param bool  $test Whether the test mode is active for essayer unit-testing.
   *
   * @return null|Uploaded
   */
  public static function get(array $file, $test = false)
  {
    $file = static::heal($file);

    if (is_array($file) && isset($file['name']) && empty($file['name']) === false) {

      $keys = array_keys($file);
      sort($keys);

      if ($keys == self::$fileKeys) {

        if (UPLOAD_ERR_NO_FILE == $file['error']) {
          return null;
        }

        return new Uploaded($file['tmp_name'], $file['name'], $file['type'], $file['size'], $file['error'], $test);
      }
    }

    return null;
  }

  /**
   * Heals a malformed PHP $_FILES array.
   *
   * PHP has a bug that the format of the $_FILES array differs, depending on
   * whether the uploaded file fields had normal field names or array-like
   * field names ("normal" vs. "parent[child]").
   *
   * This method fixes the array to look like the "normal" $_FILES array.
   *
   * @param array $data
   *
   * @return array
   */
  public static function heal($data)
  {
    if (!is_array($data)) {
      return $data;
    }

    $keys = array_keys($data);
    sort($keys);

    if (self::$fileKeys != $keys || !isset($data['name']) || !is_array($data['name'])) {
      return $data;
    }

    $files = $data;

    foreach (self::$fileKeys as $k) {
      unset($files[$k]);
    }

    foreach (array_keys($data['name']) as $key) {
      $files[$key] = static::heal(
        array(
          'error'    => $data['error'][$key],
          'name'     => $data['name'][$key],
          'type'     => $data['type'][$key],
          'tmp_name' => $data['tmp_name'][$key],
          'size'     => $data['size'][$key]
        )
      );
    }

    return $files;
  }
}