<?php

class Fortune extends \Kumbia\ActiveRecord\LiteRecord
{
    public static function cmp($a, $b)
    {
        return $a->message <=> $b->message;
    }
}
