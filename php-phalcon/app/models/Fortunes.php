<?php


class Fortunes extends \Phalcon\Mvc\Model
{
    public $id;

    public $message;

    public function getSource() {
        return "Fortune";
    }
}