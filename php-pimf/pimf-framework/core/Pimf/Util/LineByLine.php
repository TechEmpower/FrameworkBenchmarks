<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * Process file line by line.
 *
 * This is slower than reading in the file all at once, but considerably reduces memory usage!
 *
 * <code>
 *
 * $linebyline = new LineByLine(
 *   function ($line) {
 *     // do what you want with the line.
 *    return $line;
 *   }
 * );
 *
 * $feedback = $linebyline->read('//path/to/your/file.txt');
 *
 * </code>
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class LineByLine
{
  protected $callback;

  /**
   * @param string|array|\Closure $callback
   *
   * @throws \RuntimeException If no callable callback given.
   */
  public function __construct($callback)
  {
    if (!is_callable($callback)) {
      throw new \RuntimeException('no callable given');
    }

    $this->callback = $callback;
  }

  /**
   * @param string $file     Filename or stream
   * @param bool   $feedback Should callback responses be collected.
   * @param bool   $binary   Is binary file.
   *
   * @return array List of collected responses.
   * @throws \RuntimeException If can not create file handle.
   */
  public function read($file, $feedback = false, $binary = false)
  {
    if (!is_resource($handle = fopen($file, ($binary === true ? 'rb' : 'r')))) {
      throw new \RuntimeException('can not read handle');
    }

    $responses = array();

    while (!feof($handle)) {

      $response = call_user_func_array(
        $this->callback, array(fgets($handle, 4096))
      );

      if ($feedback === true) {
        $responses[] = $response;
      }
    }

    fclose($handle);

    return $responses;
  }
}
