<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * The identity map pattern is a database access design pattern used to improve performance
 * by providing a context-specific, in-memory cache to prevent duplicate retrieval of the
 * same object data from the database.
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class IdentityMap
{
  /**
   * @var \ArrayObject
   */
  protected $idToObject;

  /**
   * @var \SplObjectStorage
   */
  protected $objectToId;

  public function __construct()
  {
    $this->objectToId = new \SplObjectStorage();
    $this->idToObject = new \ArrayObject();
  }

  /**
   * @param integer $key
   * @param mixed   $object
   */
  public function set($key, $object)
  {
    $this->idToObject[$key]    = $object;
    $this->objectToId[$object] = $key;
  }

  /**
   * @param mixed $object
   *
   * @throws \OutOfBoundsException
   * @return integer
   */
  public function getId($object)
  {
    if (false === $this->hasObject($object)) {
      throw new \OutOfBoundsException('no object=' . get_class($object) . ' at the identity-map');
    }

    return $this->objectToId[$object];
  }

  /**
   * @param integer $key
   *
   * @return boolean
   */
  public function hasId($key)
  {
    return isset($this->idToObject[$key]);
  }

  /**
   * @param mixed $object
   *
   * @return boolean
   */
  public function hasObject($object)
  {
    return isset($this->objectToId[$object]);
  }

  /**
   * @param integer $key
   *
   * @throws \OutOfBoundsException
   * @return object
   */
  public function getObject($key)
  {
    if (false === $this->hasId($key)) {
      throw new \OutOfBoundsException('no id=' . $key . ' at the identity-map');
    }

    return $this->idToObject[$key];
  }
}
