<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\DataMapper\Base;

/**
 * Based on PDO it is a general manager for data persistence and object relational mapping.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 */
class EntityManager extends Base
{
  /**
   * @var string The namespace name of data-mappers repository.
   */
  protected $prefix;

  /**
   * @param \PDO   $pdo
   * @param string $prefix The data-mappers repository name.
   */
  public function __construct(\PDO $pdo, $prefix = '\Pimf')
  {
    parent::__construct($pdo);
    $this->prefix = $prefix . '\DataMapper\\';
  }

  /**
   * @param string $entity The name of the data-mapper class.
   *
   * @return Base
   * @throws \BadMethodCallException If no entity fount at the repository.
   */
  public function load($entity)
  {
    $entity = $this->prefix . ucfirst($entity);

    if (true === $this->identityMap->hasId($entity)) {
      return $this->identityMap->getObject($entity);
    }

    if (!class_exists($entity)) {
      throw new \BadMethodCallException('entity "' . $entity . '" found at the data-mapper repository');
    }

    $model = new $entity($this->pdo);

    $this->identityMap->set($entity, $model);

    return $this->identityMap->getObject($entity);
  }

  /**
   * @return bool
   */
  public function beginTransaction()
  {
    return $this->pdo->beginTransaction();
  }

  /**
   * @return bool
   */
  public function commitTransaction()
  {
    return $this->pdo->commit();
  }

  /**
   * @return bool
   */
  public function rollbackTransaction()
  {
    return $this->pdo->rollBack();
  }

  /**
   * @param string $entity
   *
   * @return Base
   */
  public function __get($entity)
  {
    return $this->load($entity);
  }

  /**
   * @return Database
   */
  public function getPDO()
  {
    return $this->pdo;
  }
}
