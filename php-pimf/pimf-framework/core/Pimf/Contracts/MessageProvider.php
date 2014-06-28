<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Contracts;

/**
 * A simply interface to get messages for the instance.
 *
 * @package Contracts
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
interface MessageProvider
{
  /**
   * Get the messages for the instance.
   *
   * @return \Pimf\Util\Message[]
   */
  public function getMessages();

}
