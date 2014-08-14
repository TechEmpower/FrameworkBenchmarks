<?php

namespace Doctrine\Tests\Common\Proxy;

/**
 * Test asset class
 */
class MagicWakeupClass
{
    /**
     * @var string
     */
    public $id = 'id';

    /**
     * @var string
     */
    public $publicField = 'publicField';

    /**
     * @var string
     */
    public $wakeupValue = 'defaultValue';

    /**
     * @return void
     */
    public function __wakeup()
    {
        $this->wakeupValue = 'newWakeupValue';
    }
}
