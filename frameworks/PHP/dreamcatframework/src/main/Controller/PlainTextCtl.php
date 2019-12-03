<?php

namespace DreamCat\Benchmark\Controller;

use Zend\Diactoros\Response\TextResponse;

class PlainTextCtl
{
    public function index()
    {
        return new TextResponse("Hello, World!");
    }
}

# end of file
