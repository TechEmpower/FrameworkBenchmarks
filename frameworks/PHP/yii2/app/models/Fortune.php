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

    public static function cmp(Fortune $left, Fortune $right): int
    {
        return \strcmp($left->message, $right->message);
    }
}
