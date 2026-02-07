<?php

use Workerman\Timer;

class Date
{
    public $date;
    public function __construct()
    {
        $this->date = gmdate(DATE_RFC7231);
        Timer::add(1, function() {
            $this->date = gmdate(DATE_RFC7231);
        });
    }
}