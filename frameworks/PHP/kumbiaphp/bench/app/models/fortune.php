<?php

class Fortune extends \Kumbia\ActiveRecord\LiteRecord
{
    public static function all()
    {
        return parent::all('SELECT id, message FROM Fortune');
    }
}