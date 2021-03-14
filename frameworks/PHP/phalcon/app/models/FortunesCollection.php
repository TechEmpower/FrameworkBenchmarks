<?php

use Phalcon\Mvc\Collection;

class FortunesCollection extends Collection
{
    public $_id;
    public $message;

    public function getSource()
    {
        return "fortune";
    }
}
