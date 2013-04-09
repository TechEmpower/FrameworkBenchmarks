<?php

namespace Doctrine\Tests\Common;

use Doctrine\Common\ClassLoader;

class ClassLoaderTest extends \Doctrine\Tests\DoctrineTestCase
{
    public function testClassLoader()
    {
        $classLoader = new ClassLoader('ClassLoaderTest');
        $classLoader->setIncludePath(__DIR__);
        $classLoader->setFileExtension('.class.php');
        $classLoader->setNamespaceSeparator('_');

        $this->assertTrue($classLoader->canLoadClass('ClassLoaderTest_ClassA'));
        $this->assertTrue($classLoader->canLoadClass('ClassLoaderTest_ClassB'));
        $this->assertTrue($classLoader->canLoadClass('ClassLoaderTest_ClassC'));
        $this->assertFalse($classLoader->canLoadClass('OtherClass'));
        $this->assertEquals($classLoader->loadClass('ClassLoaderTest_ClassA'), true);
        $this->assertEquals($classLoader->loadClass('ClassLoaderTest_ClassB'), true);
        $this->assertEquals($classLoader->loadClass('ClassLoaderTest_ClassC'), true);
    }

    public function testClassExists()
    {
        $this->assertFalse(ClassLoader::classExists('ClassLoaderTest\ClassD'));
        $badLoader = function($className) {
            require __DIR__ . '/ClassLoaderTest/ClassD.php';
            return true;
        };
        spl_autoload_register($badLoader);
        $this->assertTrue(ClassLoader::classExists('ClassLoaderTest\ClassD'));
        spl_autoload_unregister($badLoader);
    }

    public function testGetClassLoader()
    {
        $cl = new ClassLoader('ClassLoaderTest', __DIR__);
        $cl->register();
        $this->assertTrue(ClassLoader::getClassLoader('ClassLoaderTest\ClassD') instanceof \Doctrine\Common\ClassLoader);
        $this->assertNull(ClassLoader::getClassLoader('This\Class\Does\Not\Exist'));
        $cl->unregister();
    }
}
