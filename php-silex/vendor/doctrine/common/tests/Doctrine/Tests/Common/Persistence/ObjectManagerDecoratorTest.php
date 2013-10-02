<?php

namespace Doctrine\Tests\Common\Persistence;

use Doctrine\Common\Persistence\ObjectManagerDecorator;
use Doctrine\Common\Persistence\ObjectManager;

class NullObjectManagerDecorator extends ObjectManagerDecorator
{
    public function __construct(ObjectManager $wrapped)
    {
        $this->wrapped = $wrapped;
    }
}

class ObjectManagerDecoratorTest extends \PHPUnit_Framework_TestCase
{
    private $wrapped;
    private $decorated;

    public function setUp()
    {
        $this->wrapped = $this->getMock('Doctrine\Common\Persistence\ObjectManager');
        $this->decorated = new NullObjectManagerDecorator($this->wrapped);
    }

    public function getMethodParameters()
    {
        $class = new \ReflectionClass('Doctrine\Common\Persistence\ObjectManager');

        $methods = array();
        foreach ($class->getMethods() as $method) {
            if ($method->getNumberOfRequiredParameters() === 0) {
               $methods[] = array($method->getName(), array());
            } elseif ($method->getNumberOfRequiredParameters() > 0) {
                $methods[] = array($method->getName(), array_fill(0, $method->getNumberOfRequiredParameters(), 'req') ?: array());
            }
            if ($method->getNumberOfParameters() != $method->getNumberOfRequiredParameters()) {
                $methods[] = array($method->getName(), array_fill(0, $method->getNumberOfParameters(), 'all') ?: array());
            }
        }

        return $methods;
    }

    /**
     * @dataProvider getMethodParameters
     */
    public function testAllMethodCallsAreDelegatedToTheWrappedInstance($method, array $parameters)
    {
        $stub = $this->wrapped
            ->expects($this->once())
            ->method($method)
            ->will($this->returnValue('INNER VALUE FROM ' . $method));

        call_user_func_array(array($stub, 'with'), $parameters);

        $this->assertSame('INNER VALUE FROM ' . $method, call_user_func_array(array($this->decorated, $method), $parameters));
    }
}