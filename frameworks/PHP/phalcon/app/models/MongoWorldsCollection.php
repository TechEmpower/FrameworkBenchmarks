<?php

class MongoWorldsCollection extends \Phalcon\Mvc\MongoCollection
{

    public $_id;
    public $randomNumber;

    public function getSource()
    {
        return "world";
    }

}