<?php

class WorldsCollection extends \Phalcon\Mvc\Collection
{

    public $_id;
    public $randomNumber;

    public function getSource()
    {
        return "world";
    }

}