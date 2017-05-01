<?php

class MongoFortunesCollection extends \Phalcon\Mvc\MongoCollection
{

    public $_id;
    public $message;

    public function getSource()
    {
        return "fortune";
    }

}