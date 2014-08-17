<?php

class Model_Fortune extends Orm\Model
{
    protected static $_primary_key = array('id');
    protected static $_properties = array('id', 'message');
    protected static $_table_name = 'Fortune';
}