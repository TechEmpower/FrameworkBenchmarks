<?php


class Worlds extends \Phalcon\Mvc\Model
{
    public $id;

    public $randomNumber;

    public function getSource() {
        return "World";
    }
}