<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Secure;

interface TestInterface
{
    /**
     * @Secure
     */
    function foo();
}