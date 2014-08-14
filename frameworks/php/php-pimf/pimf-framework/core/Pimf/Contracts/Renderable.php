<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Contracts;

/**
 * A simply interface to give the view-adapters teh contents of the object.
 *
 * @package Contracts
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
interface Renderable
{
  /**
   * Get the evaluated contents of the object.
   *
   * @return string
   */
  public function render();

}
