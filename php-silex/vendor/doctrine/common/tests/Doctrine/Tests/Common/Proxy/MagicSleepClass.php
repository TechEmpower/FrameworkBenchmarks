<?php

namespace Doctrine\Tests\Common\Proxy;

/**
 * Test asset class
 */
class MagicSleepClass
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
    public $serializedField = 'defaultValue';

    /**
     * @var string
     */
    public $nonSerializedField = 'defaultValue';

    /**
     * @return array
     */
    public function __sleep()
    {
        return array('serializedField');
    }
}
