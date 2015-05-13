<?php

if (!defined('CF_SYSTEM')) {
    exit('No External script access allowed');
}
/**
 * Direct routing to controller
 * This file is used to set all routing configurations
 */
return array(
    '/sayhello/{:name}' => 'user.welcome',
    //'/blog(/{:year}(/{:month}(/{:day}?)?)?)?' => 'home.category'
);
