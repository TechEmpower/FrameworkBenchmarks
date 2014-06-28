<?php
namespace Vanilla\DataMapper;

use Pimf\DataMapper\Base;

class World extends Base
{
  /**
   * @param integer $id
   *
   * @return array
   */
  public function find($id)
  {
    $sth = $this->pdo->prepare('SELECT * FROM World WHERE id = :id');

    $sth->bindValue(':id', $id, \PDO::PARAM_INT);
    $sth->execute();

    return (array) $sth->fetch(\PDO::FETCH_ASSOC);
  }

  /**
   * @param array $world
   *
   * @return bool
   */
  public function update(array $world)
  {
    $sth = $this->pdo->prepare('UPDATE World SET randomNumber = :randomNumber WHERE id = :id');

    $sth->bindValue(':randomNumber', $world['randomNumber'], \PDO::PARAM_INT);
    $sth->bindValue(':id', $world['id'], \PDO::PARAM_INT);

    return $sth->execute();
  }
}
