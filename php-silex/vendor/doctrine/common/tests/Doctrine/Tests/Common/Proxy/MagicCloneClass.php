<?php

namespace Doctrine\Tests\Common\Proxy;

/**
 * Test asset class
 */
class MagicCloneClass
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
     * @var boolean
     */
    public $cloned = false;

    /**
     * @var string
     */
    public $clonedValue = 'defaultValue';

    /**
     * @return void
     */
    public function __clone()
    {
        $this->clonedValue = 'newClonedValue';
    }
}
