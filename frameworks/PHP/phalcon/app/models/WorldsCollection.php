<?php

use Phalcon\Mvc\Collection;

class WorldsCollection extends Collection
{
    public $_id;
    public $randomNumber;

    public function getSource()
    {
        return "world";
    }
}
