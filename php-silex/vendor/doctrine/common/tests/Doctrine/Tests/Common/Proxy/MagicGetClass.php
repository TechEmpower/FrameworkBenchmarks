<?php

namespace Doctrine\Tests\Common\Proxy;

/**
 * Test asset class
 */
class MagicGetClass
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
     * @param $name
     *
     * @return string
     * @throws \BadMethodCallException
     */
    public function __get($name)
    {
        if ($name === 'test') {
            return 'test';
        }

        if ($name === 'publicField' || $name === 'id') {
            throw new \BadMethodCallException('Should never be called for "publicField" or "id"');
        }

        return 'not defined';
    }
}
