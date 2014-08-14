<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * String
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class String
{
  /**
   * Check value to find if it was serialized.
   *
   * @param   mixed $string Value to check to see if was serialized
   *
   * @return  bool
   */
  public static function isSerialized($string)
  {
    return (@unserialize($string) !== false || $string == 'b:0;');
  }

  /**
   * Check for invalid UTF8 encoding and invalid byte .
   *
   * @param string $string Your string.
   *
   * @return boolean
   */
  public static function checkUtf8Encoding($string)
  {
    if (!mb_check_encoding($string, 'UTF-8') or
      !$string == mb_convert_encoding(mb_convert_encoding($string, 'UTF-32', 'UTF-8'), 'UTF-8', 'UTF-32')
    ) {
      return false;
    }

    return true;
  }

  /**
   * Ensure that a string is ends with a special string.
   *
   * <code>
   * - ensureTrailing('/', 'http://www.example.com') -> 'http://www.example.com/'
   * - ensureTrailing('/', 'http://www.example.com/') -> 'http://www.example.com/'
   * </code>
   *
   * @param string $needle   The needle.
   * @param string $haystack The haystack.
   *
   * @return string
   */
  public static function ensureTrailing($needle, $haystack)
  {
    $needleLength = strlen($needle);
    $needlePart   = substr($haystack, -1 * $needleLength);

    if ($needlePart !== $needle) {
      // append missing trailing character.
      $haystack .= $needle;
    }

    return $haystack;
  }

  /**
   * Ensure that a string is starts with a special string.
   *
   * <code>
   * - ensureLeading('#', '1#2#3#4#5') -> '#1#2#3#4#5'
   * - ensureLeading('#', '#1#2#3#4#5') -> '#1#2#3#4#5'
   * </code>
   *
   * @param string $needle   The needle.
   * @param string $haystack The haystack
   *
   * @return string
   */
  public static function ensureLeading($needle, $haystack)
  {
    $needleLength = strlen($needle);
    $needlePart   = substr($haystack, 0, $needleLength);

    if ($needlePart !== $needle) {
      // append missing trailing string
      $haystack = $needle . $haystack;
    }

    return $haystack;
  }

  /**
   * Delete trailing characters.
   *
   * <code>
   * - deleteTrailing('|', '|1|2|3|4|5|')               -> '|1|2|3|4|5'
   * - deleteTrailing(array('|','5'), '|1|2|3|4|5|555') -> '|1|2|3|4'
   * </code>
   *
   * @param string|array $needle   The needle.
   * @param string       $haystack The haystack.
   *
   * @return string
   */
  public static function deleteTrailing($needle, $haystack)
  {
    $pattern = '#(' . self::pregQuote($needle, '#') . ')+$#';
    $result  = preg_replace($pattern, '', $haystack);

    return $result;
  }

  /**
   * Delete leading characters.
   *
   * <code>
   * - deleteTrailing('#', '#1#2#3#4#5')             -> '1#2#3#4#5'
   * - deleteTrailing(array('#', '1'), '##11#2#3#4#5') -> '2#3#4#5'
   * </code>
   *
   * @param string|array $needle   The needle.
   * @param string       $haystack The haystack.
   *
   * @return string
   */
  public static function deleteLeading($needle, $haystack)
  {
    $pattern = '#^(' . self::pregQuote($needle, '#') . ')+#';
    $result  = preg_replace($pattern, '', $haystack);

    return $result;
  }

  /**
   * Wrapper for preg_quote supporting strings and array of strings.
   *
   * @param mixed       $values    The values.
   * @param null|string $delimiter (Optional) The delimiter.
   *
   * @return string
   */
  public static function pregQuote($values, $delimiter = null)
  {
    if (!is_array($values)) {
      return preg_quote($values, $delimiter);
    }

    // Case: needle is array
    foreach ($values as $key => $value) {
      $values[$key] = preg_quote($value, $delimiter);
    }

    return implode('|', $values);
  }

  /**
   * An aggressive cleaning - all tags and stuff inside will be removed.
   *
   * @param string $string The string.
   *
   * @return string
   */
  public static function cleanAggressive($string)
  {
    return \Pimf\Util\String\Clean::aggressive($string);
  }

  /**
   * Cleans against XSS.
   * Info: use it on showing your request data.
   *
   * @param string $string  String to check
   * @param string $charset Character set (default ISO-8859-1)
   *
   * @return string $value Sanitized string
   */
  public static function cleanXss($string, $charset = 'ISO-8859-1')
  {
    return \Pimf\Util\String\Clean::xss($string, $charset);
  }

  /**
   * @param int $length
   *
   * @return string
   */
  public static function random($length = 32)
  {
    return substr(
      str_shuffle(str_repeat('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ', 5)), 0, $length
    );
  }

  /**
   * Determine if a given string contains a given sub-string.
   *
   * @param string       $haystack
   * @param string|array $needle
   *
   * @return bool
   */
  public static function contains($haystack, $needle)
  {
    foreach ((array)$needle as $n) {
      if (strpos($haystack, $n) !== false) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determine if a given string begins with a given value.
   *
   * @param string $haystack
   * @param string $needle
   *
   * @return bool
   */
  public static function startsWith($haystack, $needle)
  {
    return strpos($haystack, $needle) === 0;
  }

  /**
   * Determine if a given string ends with a given value.
   *
   * @param string $haystack
   * @param string $needle
   *
   * @return bool
   */
  public static function endsWith($haystack, $needle)
  {
    return $needle == substr($haystack, strlen($haystack) - strlen($needle));
  }

  /**
   * Determine if a given string matches a given pattern.
   *
   * Asterisks are translated into zero-or-more regular expression wildcards
   * to make it convenient to check if string such as "library/*".
   *
   * @param  string $pattern Pattern or wildcard
   * @param  string $value
   *
   * @return bool
   */
  public static function is($pattern, $value)
  {
    if ($pattern !== '/') {
      $pattern = str_replace('*', '(.*)', $pattern) . '\z';
    } else {
      $pattern = '^/$';
    }

    return (bool)preg_match('#' . $pattern . '#', $value);
  }

  /**
   * Check if strange things happening.
   *
   * @param string $path
   *
   * @return bool
   */
  public static function isEvilPath($path)
  {
    return self::contains($path, array('../', "..\\", '/..', '\..', ' ', '%2e%2e%2f', '%2e%2e%5C', '%2F%2e%2e', '%5C%2e%2e', '%20'));
  }
}
