<?php

namespace Doctrine\Tests\Common\Reflection;

use PHPUnit_Framework_TestCase;
use Doctrine\Common\Reflection\RuntimePublicReflectionProperty;
use Doctrine\Common\Proxy\Proxy;

class RuntimePublicReflectionPropertyTest extends PHPUnit_Framework_TestCase
{
    public function testGetValueOnProxyPublicProperty()
    {
        $getCheckMock = $this->getMock('stdClass', array('callGet'));
        $getCheckMock->expects($this->never())->method('callGet');
        $initializer = function () use ($getCheckMock) {
            call_user_func($getCheckMock);
        };

        $mockProxy = new RuntimePublicReflectionPropertyTestProxyMock();
        $mockProxy->__setInitializer($initializer);

        $reflProperty = new RuntimePublicReflectionProperty(
            __NAMESPACE__ . '\RuntimePublicReflectionPropertyTestProxyMock',
            'checkedProperty'
        );

        $this->assertSame('testValue', $reflProperty->getValue($mockProxy));
        unset($mockProxy->checkedProperty);
        $this->assertNull($reflProperty->getValue($mockProxy));
    }

    public function testSetValueOnProxyPublicProperty()
    {
        $setCheckMock = $this->getMock('stdClass', array('neverCallSet'));
        $setCheckMock->expects($this->never())->method('neverCallSet');
        $initializer = function () use ($setCheckMock) {
            call_user_func(array($setCheckMock, 'neverCallSet'));
        };

        $mockProxy = new RuntimePublicReflectionPropertyTestProxyMock();
        $mockProxy->__setInitializer($initializer);

        $reflProperty = new RuntimePublicReflectionProperty(
            __NAMESPACE__ . '\RuntimePublicReflectionPropertyTestProxyMock',
            'checkedProperty'
        );

        $reflProperty->setValue($mockProxy, 'newValue');
        $this->assertSame('newValue', $mockProxy->checkedProperty);

        unset($mockProxy->checkedProperty);
        $reflProperty->setValue($mockProxy, 'otherNewValue');
        $this->assertSame('otherNewValue', $mockProxy->checkedProperty);

        $setCheckMock = $this->getMock('stdClass', array('callSet'));
        $setCheckMock->expects($this->once())->method('callSet');
        $initializer = function () use ($setCheckMock) {
            call_user_func(array($setCheckMock, 'callSet'));
        };

        $mockProxy->__setInitializer($initializer);
        $mockProxy->__setInitialized(true);

        unset($mockProxy->checkedProperty);
        $reflProperty->setValue($mockProxy, 'againNewValue');
        $this->assertSame('againNewValue', $mockProxy->checkedProperty);
    }
}

/**
 * Mock that simulates proxy public property lazy loading
 */
class RuntimePublicReflectionPropertyTestProxyMock implements Proxy
{
    /**
     * @var \Closure|null
     */
    private $initializer     = null;

    /**
     * @var \Closure|null
     */
    private $initialized     = false;

    /**
     * @var string
     */
    public  $checkedProperty = 'testValue';

    /**
     * {@inheritDoc}
     */
    public function __getInitializer()
    {
        return $this->initializer;
    }

    /**
     * {@inheritDoc}
     */
    public function __setInitializer(\Closure $initializer = null)
    {
        $this->initializer = $initializer;
    }

    /**
     * {@inheritDoc}
     */
    public function __getLazyProperties()
    {
    }

    /**
     * {@inheritDoc}
     */
    public function __load()
    {
    }

    /**
     * {@inheritDoc}
     */
    public function __isInitialized()
    {
        return $this->initialized;
    }

    /**
     * {@inheritDoc}
     */
    public function __setInitialized($initialized)
    {
        $this->initialized = (bool) $initialized;
    }

    /**
     * @param string $name
     */
    public function __get($name)
    {
        if ($this->initializer) {
            $cb = $this->initializer;
            $cb();
        }

        return $this->checkedProperty;
    }

    /**
     * @param string $name
     * @param mixed  $value
     */
    public function __set($name, $value)
    {
        if ($this->initializer) {
            $cb = $this->initializer;
            $cb();
        }

        // triggers notices if `$name` is used: see https://bugs.php.net/bug.php?id=63463
        $this->checkedProperty = $value;
    }

    /**
     * @param string $name
     *
     * @return integer
     */
    public function __isset($name)
    {
        if ($this->initializer) {
            $cb = $this->initializer;
            $cb();
        }

        return isset($this->checkedProperty);
    }

    /**
     * {@inheritDoc}
     */
    public function __setCloner(\Closure $cloner = null)
    {
    }

    /**
     * {@inheritDoc}
     */
    public function __getCloner()
    {
    }
}