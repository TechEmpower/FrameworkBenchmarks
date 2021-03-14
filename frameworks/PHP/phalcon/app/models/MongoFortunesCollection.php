<?php

use Phalcon\Mvc\MongoCollection;

class MongoFortunesCollection extends MongoCollection
{
    public $_id;
    public $message;

    public function getSource()
    {
        return "fortune";
    }
}
