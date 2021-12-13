<?php

class World extends \Kumbia\ActiveRecord\LiteRecord
{
    public static function byId($id)
    {
        return self::first('SELECT * FROM World WHERE id = ?', [$id]);
    }
}
