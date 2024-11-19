<?php

use Workerman\Timer;

class Date
{
    public $date = null;
    public function __construct()
    {
        $this->date = gmdate('D, d M Y H:i:s').' GMT';
        Timer::add(1, function() {
            $this->date = gmdate('D, d M Y H:i:s').' GMT';
        });
    }
}