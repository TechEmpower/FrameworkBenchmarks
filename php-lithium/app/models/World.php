<?php

namespace app\models;

use \lithium\data\Model;

class World extends Model {
    // stop lithium from pluralizing the table name
    protected $_meta = array(
        'source' => 'World'
    );
}