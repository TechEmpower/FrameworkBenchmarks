<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Contracts;

/**
 * A simply interface to delete all expired data from persistent storage of the instance.
 *
 * @package Contracts
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
interface Cleanable
{
  /**
   * Delete all expired instance-data from persistent storage.
   *
   * @param int $expiration
   *
   * @return mixed
   */
  public function clean($expiration);

}
