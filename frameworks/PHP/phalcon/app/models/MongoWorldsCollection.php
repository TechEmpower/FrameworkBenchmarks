<?php

use Phalcon\Mvc\MongoCollection;

class MongoWorldsCollection extends MongoCollection
{
    public $_id;
    public $randomNumber;

    public function getSource()
    {
        return "world";
    }
}
