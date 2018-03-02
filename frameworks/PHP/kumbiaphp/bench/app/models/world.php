<?php

class World extends \Kumbia\ActiveRecord\LiteRecord
{
    public static function byId($id)
    {
        return self::first('SELECT id, randomNumber FROM World WHERE id = ?', $id);
    }
}
