<?php

class Model_World extends Orm\Model
{
    protected static $_primary_key = array('id');
    protected static $_properties = array('id', 'randomNumber');
    protected static $_table_name = 'World';

    public function toJson() {
        return array(
            'id' => $this->id,
            'randomNumber' => $this->randomNumber
        );
    }
}