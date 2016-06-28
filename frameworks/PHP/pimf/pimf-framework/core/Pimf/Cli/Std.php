<?php
/**
 * Cli
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Cli;

/**
 * Responsible for accessing I/O streams that allow access to PHP's own input and output streams.
 *
 * @package Cli
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Std
{
  /**
   * @var resource
   */
  private $handle;

  /**
   * @param string $stream
   */
  public function __construct($stream = 'php://stdin')
  {
    $this->handle = fopen($stream, 'r');
  }

  /**
   * @return string
   */
  public function value()
  {
    return substr(fgets($this->handle, 1024), 0, -1);
  }

  public function __destruct()
  {
    fclose($this->handle);
  }

  /**
   * Allow direct access to the corresponding input stream of the PHP process.
   *
   * @param string $prompt
   * @param string $validation A regex pattern
   *
   * <code>
   *
   *  Have a look at the examples for $validation:
   *
   *  Regular Expression  | Will match...
   *  -------------------------------------------------------------
   *  .*                  | Not empty
   *  foo                 | The string "foo"
   *  ^foo                | "foo" at the start of a string
   *  foo$                | "foo" at the end of a string
   *  ^foo$               | "foo" when it is alone on a string
   *  [abc]               | a, b, or c
   *  [a-z]               | Any lowercase letter
   *  [^A-Z]              | Any character that is not a uppercase letter
   *  (gif|jpg)           | Matches either "gif" or "jpeg"
   *  [a-z]+              | One or more lowercase letters
   *  [0-9\.\-]           | Аny number, dot, or minus sign
   *
   * </code>
   *
   * @return string
   */
  public function read($prompt, $validation = "/.*/")
  {
    $value = '';

    while (true) {

      echo "Please enter a " . $prompt . ":\n";

      $value = $this->value();

      if ($this->valid($validation, $value)) {
        break;
      }

      echo "[ Value format for " . $prompt . " is invalid! ]\n";
    }

    return $value;
  }

  /**
   * @param string $validation A regex pattern
   * @param string $value
   *
   * @return bool
   */
  public function valid($validation, $value)
  {
    return strlen($value) > 0 && preg_match($validation, $value);
  }
}
