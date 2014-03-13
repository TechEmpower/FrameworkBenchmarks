<?php
namespace Vanilla\DataMapper;

use Pimf\DataMapper\Base;

class Fortune extends Base
{
  /**
   * @return array
   */
  public function getAll()
  {
    $sth = $this->pdo->prepare('SELECT * FROM Fortune');
    $sth->execute();

    return $sth->fetchAll();
  }
}
 