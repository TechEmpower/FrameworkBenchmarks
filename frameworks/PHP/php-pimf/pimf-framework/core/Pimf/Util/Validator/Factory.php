<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util\Validator;

use Pimf\Param;
use Pimf\Util\Validator;

/**
 * Validator Factory
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class Factory
{
  /**
   * <code>
   *
   *  $attributes = array(
   *    'fname'    => 'conan',
   *    'age'      => 33,
   *   );
   *
   *   $rules = array(
   *     'fname'   => 'alpha|length[>,0]|lengthBetween[1,9]',
   *     'age'     => 'digit|value[>,18]|value[==,33]',
   *   );
   *
   *  $validator = Validator::factory($attributes, $rules);
   *
   * </code>
   *
   * @param array|\Pimf\Param $attributes
   * @param array             $rules
   *
   * @return \Pimf\Util\Validator
   */
  public static function get($attributes, array $rules)
  {
    if (!($attributes instanceof Param)) {
      $attributes = new Param((array)$attributes);
    }

    $validator = new Validator($attributes);

    foreach ($rules as $key => $rule) {

      $checks = (is_string($rule)) ? explode('|', $rule) : $rule;

      foreach ($checks as $check) {

        $items      = explode('[', str_replace(']', '', $check));
        $method     = $items[0];
        $parameters = array_merge(array($key), (isset($items[1]) ? explode(',', $items[1]) : array()));

        call_user_func_array(array($validator, $method), $parameters);
      }
    }

    return $validator;
  }
}
