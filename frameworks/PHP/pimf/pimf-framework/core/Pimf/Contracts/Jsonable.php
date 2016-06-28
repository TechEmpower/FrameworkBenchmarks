<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Contracts;

/**
 * A simply interface to get instance to its JSON representation.
 *
 * @package Contracts
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
interface Jsonable
{
  /**
   * Convert the object to its JSON representation.
   *
   * @param  int $options
   *
   * @return string
   */
  public function toJson($options = 0);

}
