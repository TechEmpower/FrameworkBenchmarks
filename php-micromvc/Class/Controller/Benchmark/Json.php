<?php

namespace Controller\Benchmark;

class Json extends \MyController
{
    public function run(){}

    public function send()
    {
        headers_sent() OR header('Content-type: application/json');
        echo json_encode(array('message' => 'Hello World'));
    }
}
