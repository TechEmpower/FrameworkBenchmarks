<?php

namespace app\models;

use yii\db\ActiveRecord;

/**
 * @property $id
 * @property $message
 */
class Fortune extends ActiveRecord
{
    public static function tableName()
    {
        return 'fortune';
    }
}
