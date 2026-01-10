<?php
namespace app\modules\site\models;

use Piko\DbRecord\Attribute\Table;
use Piko\DbRecord\Attribute\Column;

/**
 * This is the model class for table "Fortune".
 */
#[Table(name: 'Fortune')]
class Fortune extends \Piko\DbRecord
{
    #[Column(primaryKey: true)]
    public ?int $id = null;

    #[Column]
    public string $message = '';
}
