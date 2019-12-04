<?php

namespace DreamCat\Benchmark\Controller;

use Zend\Diactoros\Response\TextResponse;

/**
 * plain text 测试控制器
 * @author vijay
 */
class PlainTextCtl
{
    /**
     * 返回hello world
     * @return TextResponse
     */
    public function index()
    {
        return new TextResponse("Hello, World!");
    }
}

# end of file
