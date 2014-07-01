<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Session\Storages;

use Pimf\Contracts\Cleanable;

/**
 * @package Session_Storages
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Pdo extends Storage implements Cleanable
{
  /**
   * @var \Pimf\Database
   */
  protected $pdo;

  /**
   * @param \Pimf\Database $pdo
   */
  public function __construct(\Pimf\Database $pdo)
  {
    $this->pdo = $pdo;
  }

  /**
   * Load a session from storage by a given ID.
   * If no session is found for the ID, null will be returned.
   *
   * @param string $key
   *
   * @return array|null
   */
  public function load($key)
  {
    try {
      $sth = $this->pdo->prepare(
        'SELECT * FROM sessions WHERE id = :id'
      );

      $sth->bindValue(':id', $key, \PDO::PARAM_INT);
      $sth->execute();

      $session = $sth->fetchObject();

      if ($session instanceof \stdClass) {
        return array('id' => $session->id, 'last_activity' => $session->last_activity, 'data' => unserialize($session->data));
      }
    } catch (\PDOException $pdoe) {
      return null;
    }
  }

  /**
   * Save a given session to storage.
   *
   * @param array $session
   * @param array $config
   * @param bool  $exists
   */
  public function save($session, $config, $exists)
  {
    if ($exists) {
      $sth = $this->pdo->prepare(
        "INSERT INTO sessions (id, last_activity, data) VALUES (:id, :last_activity, :data)"
      );
    } else {
      $sth = $this->pdo->prepare(
        "UPDATE sessions SET last_activity = :last_activity, data = :data WHERE id = :id"
      );
    }

    $sth->bindValue(':id', $session['id'], \PDO::PARAM_INT);
    $sth->bindValue(':last_activity', $session['last_activity']);
    $sth->bindValue(':data', serialize($session['data']));
    $sth->execute();
  }

  /**
   * Delete a session from storage by a given ID.
   *
   * @param string $key
   */
  public function delete($key)
  {
    $sth = $this->pdo->prepare(
      "DELETE FROM sessions WHERE id = :id"
    );

    $sth->bindValue(':id', $key, \PDO::PARAM_INT);
    $sth->execute();
  }

  /**
   * Delete all expired sessions from persistent storage.
   *
   * @param int $expiration
   *
   * @return mixed|void
   */
  public function clean($expiration)
  {
    $sth = $this->pdo->prepare(
      "DELETE FROM sessions WHERE last_activity < :expiration"
    );

    $sth->bindValue(':expiration', $expiration);
    $sth->execute();
  }
}
