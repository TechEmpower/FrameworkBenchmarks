<?php

namespace Doctrine\Tests\Common\Proxy;

/**
 * Test asset class
 */
class SleepClass
{
    public $id;

    /**
     * @return array
     */
    public function __sleep()
    {
        return array('id');
    }
}
