<?php
namespace app\modules\site\models;

use Piko\DbRecord\Attribute\Table;
use Piko\DbRecord\Attribute\Column;

/**
 * This is the model class for table "World".
 */
#[Table(name: 'World')]
class World extends \Piko\DbRecord
{
    #[Column(primaryKey: true)]
    public ?int $id = null;

    #[Column]
    public int $randomnumber = 0;
    // The var name is not 'randomNumber' because the column name doesn't exists in the pgsql table
    // For Mysql, there is no difference as it is not case sensitive.
}
