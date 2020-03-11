<?php

class Worlds extends \Phalcon\Mvc\Model
{
    public $id;

    public $randomNumber;

    public function initialize()
    {
        $this->setSource('World');
    }
}