<?php

namespace Doctrine\Tests\Common\Proxy;

/**
 * Test asset class
 */
class InvalidTypeHintClass
{
    /**
     * @param InvalidHint (non existing class type hint)
     */
    public function invalidTypeHintMethod(InvalidHint $foo)
    {
    }
}
