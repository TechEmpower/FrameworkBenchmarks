<?php
/**
 * DataMapper
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\DataMapper;

use Pimf\Util\IdentityMap;

/**
 * For mapping the domain models to the persistence layer.
 *
 * Defines the general behaviour for the data-mappers - you have to extend it.
 *
 * You have to use it if you want to persist data.
 *
 * @package DataMapper
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 * @method insert($entity)
 * @method update($entity)
 * @method delete($entity)
 * @method find($key)
 */
abstract class Base
{
  /**
   * @var \PDO The database resource.
   */
  protected $pdo;

  /**
   * @var \Pimf\Util\IdentityMap
   */
  protected $identityMap;

  /**
   * @param \PDO $pdo
   */
  public function __construct(\PDO $pdo)
  {
    $this->pdo         = $pdo;
    $this->identityMap = new IdentityMap();
  }

  public function __destruct()
  {
    unset($this->identityMap, $this->pdo);
  }

  /**
   * Makes a given model-property accessible.
   *
   * @param object $model
   * @param int    $value
   * @param string $property
   *
   * @return mixed
   */
  public function reflect($model, $value, $property = 'id')
  {
    $attribute = new \ReflectionProperty($model, $property);
    $attribute->setAccessible(true);
    $attribute->setValue($model, $value);

    return $model;
  }
}
