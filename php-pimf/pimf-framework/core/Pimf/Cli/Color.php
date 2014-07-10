<?php
/**
 * Cli
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Cli;

use Pimf\Sapi;

/**
 * For easily use ANSI console colors in your application.
 *
 * @package Cli
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Color
{
  protected static $foreground = array(
      'black'        => '0;30',
      'dark_gray'    => '1;30',
      'blue'         => '0;34',
      'light_blue'   => '1;34',
      'green'        => '0;32',
      'light_green'  => '1;32',
      'cyan'         => '0;36',
      'light_cyan'   => '1;36',
      'red'          => '0;31',
      'light_red'    => '1;31',
      'purple'       => '0;35',
      'light_purple' => '1;35',
      'brown'        => '0;33',
      'yellow'       => '1;33',
      'light_gray'   => '0;37',
      'white'        => '1;37',
    );

  protected static $background = array(
      'black'      => '40',
      'red'        => '41',
      'green'      => '42',
      'yellow'     => '43',
      'blue'       => '44',
      'magenta'    => '45',
      'cyan'       => '46',
      'light_gray' => '47',
    );

  /**
   * Returns colored string
   *
   * @param string      $string
   * @param string|null $foregroundColor
   * @param null        $backgroundColor
   *
   * @return string
   */
  public static function paint($string, $foregroundColor = 'cyan', $backgroundColor = null)
  {
    if (Sapi::isWindows()) {
      return $string;
    }

    $colored = "";

    // check if given foreground color found
    if (isset(self::$foreground[$foregroundColor])) {
      $colored .= "\033[" . self::$foreground[$foregroundColor] . "m";
    }

    // check if given background color found
    if (isset(self::$background[$backgroundColor])) {
      $colored .= "\033[" . static::$background[$backgroundColor] . "m";
    }

    // add string and end coloring
    return $colored . $string . "\033[0m";
  }
}
