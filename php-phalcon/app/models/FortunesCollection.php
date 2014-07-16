<?php

class FortunesCollection extends \Phalcon\Mvc\Collection
{

    public $_id;
    public $message;

    public function getSource()
    {
        return "fortune";
    }

}