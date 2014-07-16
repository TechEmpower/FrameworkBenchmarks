<?php

/*
 * This file is part of Pimple.
 *
 * Copyright (c) 2009 Fabien Potencier
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is furnished
 * to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

namespace Pimple\Tests;

use Pimple;

/**
 * Pimple Test
 *
 * @package pimple
 * @author  Igor Wiedler <igor@wiedler.ch>
 */
class PimpleTest extends \PHPUnit_Framework_TestCase
{
    public function testWithString()
    {
        $pimple = new Pimple();
        $pimple['param'] = 'value';

        $this->assertEquals('value', $pimple['param']);
    }

    public function testWithClosure()
    {
        $pimple = new Pimple();
        $pimple['service'] = function () {
            return new Service();
        };

        $this->assertInstanceOf('Pimple\Tests\Service', $pimple['service']);
    }

    public function testServicesShouldBeDifferent()
    {
        $pimple = new Pimple();
        $pimple['service'] = function () {
            return new Service();
        };

        $serviceOne = $pimple['service'];
        $this->assertInstanceOf('Pimple\Tests\Service', $serviceOne);

        $serviceTwo = $pimple['service'];
        $this->assertInstanceOf('Pimple\Tests\Service', $serviceTwo);

        $this->assertNotSame($serviceOne, $serviceTwo);
    }

    public function testShouldPassContainerAsParameter()
    {
        $pimple = new Pimple();
        $pimple['service'] = function () {
            return new Service();
        };
        $pimple['container'] = function ($container) {
            return $container;
        };

        $this->assertNotSame($pimple, $pimple['service']);
        $this->assertSame($pimple, $pimple['container']);
    }

    public function testIsset()
    {
        $pimple = new Pimple();
        $pimple['param'] = 'value';
        $pimple['service'] = function () {
            return new Service();
        };

        $pimple['null'] = null;

        $this->assertTrue(isset($pimple['param']));
        $this->assertTrue(isset($pimple['service']));
        $this->assertTrue(isset($pimple['null']));
        $this->assertFalse(isset($pimple['non_existent']));
    }

    public function testConstructorInjection ()
    {
        $params = array("param" => "value");
        $pimple = new Pimple($params);

        $this->assertSame($params['param'], $pimple['param']);
    }

    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage Identifier "foo" is not defined.
     */
    public function testOffsetGetValidatesKeyIsPresent()
    {
        $pimple = new Pimple();
        echo $pimple['foo'];
    }

    public function testOffsetGetHonorsNullValues()
    {
        $pimple = new Pimple();
        $pimple['foo'] = null;
        $this->assertNull($pimple['foo']);
    }

    public function testUnset()
    {
        $pimple = new Pimple();
        $pimple['param'] = 'value';
        $pimple['service'] = function () {
            return new Service();
        };

        unset($pimple['param'], $pimple['service']);
        $this->assertFalse(isset($pimple['param']));
        $this->assertFalse(isset($pimple['service']));
    }

    public function testShare()
    {
        $pimple = new Pimple();
        $pimple['shared_service'] = $pimple->share(function () {
            return new Service();
        });

        $serviceOne = $pimple['shared_service'];
        $this->assertInstanceOf('Pimple\Tests\Service', $serviceOne);

        $serviceTwo = $pimple['shared_service'];
        $this->assertInstanceOf('Pimple\Tests\Service', $serviceTwo);

        $this->assertSame($serviceOne, $serviceTwo);
    }

    public function testProtect()
    {
        $pimple = new Pimple();
        $callback = function () { return 'foo'; };
        $pimple['protected'] = $pimple->protect($callback);

        $this->assertSame($callback, $pimple['protected']);
    }

    public function testGlobalFunctionNameAsParameterValue()
    {
        $pimple = new Pimple();
        $pimple['global_function'] = 'strlen';
        $this->assertSame('strlen', $pimple['global_function']);
    }

    public function testRaw()
    {
        $pimple = new Pimple();
        $pimple['service'] = $definition = function () { return 'foo'; };
        $this->assertSame($definition, $pimple->raw('service'));
    }

    public function testRawHonorsNullValues()
    {
        $pimple = new Pimple();
        $pimple['foo'] = null;
        $this->assertNull($pimple->raw('foo'));
    }

    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage Identifier "foo" is not defined.
     */
    public function testRawValidatesKeyIsPresent()
    {
        $pimple = new Pimple();
        $pimple->raw('foo');
    }

    public function testExtend()
    {
        $pimple = new Pimple();
        $pimple['shared_service'] = $pimple->share(function () {
            return new Service();
        });

        $value = 12345;

        $pimple->extend('shared_service', function($sharedService) use ($value) {
            $sharedService->value = $value;

            return $sharedService;
        });

        $serviceOne = $pimple['shared_service'];
        $this->assertInstanceOf('Pimple\Tests\Service', $serviceOne);
        $this->assertEquals($value, $serviceOne->value);

        $serviceTwo = $pimple['shared_service'];
        $this->assertInstanceOf('Pimple\Tests\Service', $serviceTwo);
        $this->assertEquals($value, $serviceTwo->value);

        $this->assertSame($serviceOne, $serviceTwo);
    }

    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage Identifier "foo" is not defined.
     */
    public function testExtendValidatesKeyIsPresent()
    {
        $pimple = new Pimple();
        $pimple->extend('foo', function () {});
    }

    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage Identifier "foo" does not contain an object definition.
     */
    public function testExtendValidatesKeyYieldsObjectDefinition()
    {
        $pimple = new Pimple();
        $pimple['foo'] = 123;
        $pimple->extend('foo', function () {});
    }

    public function testKeys()
    {
        $pimple = new Pimple();
        $pimple['foo'] = 123;
        $pimple['bar'] = 123;

        $this->assertEquals(array('foo', 'bar'), $pimple->keys());
    }

    /** @test */
    public function settingAnInvokableObjectShouldTreatItAsFactory()
    {
        $pimple = new Pimple();
        $pimple['invokable'] = new Invokable();

        $this->assertEquals('I was invoked', $pimple['invokable']);
    }

    /** @test */
    public function settingNonInvokableObjectShouldTreatItAsParameter()
    {
        $pimple = new Pimple();
        $pimple['non_invokable'] = new NonInvokable();

        $this->assertInstanceOf('Pimple\Tests\NonInvokable', $pimple['non_invokable']);
    }
}
