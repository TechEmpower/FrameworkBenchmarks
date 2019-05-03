<?php

namespace app\models;

use yii\db\ActiveRecord;

/**
 * @property $randomNumber
 */
class World extends ActiveRecord
{
    public static function tableName()
    {
        return 'world';
    }
}
