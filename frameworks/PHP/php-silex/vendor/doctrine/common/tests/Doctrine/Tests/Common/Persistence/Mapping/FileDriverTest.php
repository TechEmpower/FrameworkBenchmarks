<?php

namespace Doctrine\Tests\Common\Persistence\Mapping;

use Doctrine\Tests\DoctrineTestCase;
use Doctrine\Common\Persistence\Mapping\Driver\FileDriver;
use Doctrine\Common\Persistence\Mapping\ClassMetadata;

class FileDriverTest extends DoctrineTestCase
{
    public function testGlobalBasename()
    {
        $driver = new TestFileDriver(array());

        $this->assertNull($driver->getGlobalBasename());

        $driver->setGlobalBasename("global");
        $this->assertEquals("global", $driver->getGlobalBasename());
    }

    public function testGetElementFromGlobalFile()
    {
        $driver = new TestFileDriver($this->newLocator());
        $driver->setGlobalBasename("global");

        $element = $driver->getElement('stdGlobal');

        $this->assertEquals('stdGlobal', $element);
    }

    public function testGetElementFromFile()
    {
        $locator = $this->newLocator();
        $locator->expects($this->once())
                ->method('findMappingFile')
                ->with($this->equalTo('stdClass'))
                ->will($this->returnValue(__DIR__ . '/_files/stdClass.yml'));

        $driver = new TestFileDriver($locator);

        $this->assertEquals('stdClass', $driver->getElement('stdClass'));
    }

    public function testGetAllClassNamesGlobalBasename()
    {
        $driver = new TestFileDriver($this->newLocator());
        $driver->setGlobalBasename("global");

        $classNames = $driver->getAllClassNames();

        $this->assertEquals(array('stdGlobal', 'stdGlobal2'), $classNames);
    }

    public function testGetAllClassNamesFromMappingFile()
    {
        $locator = $this->newLocator();
        $locator->expects($this->any())
                ->method('getAllClassNames')
                ->with($this->equalTo(null))
                ->will($this->returnValue(array('stdClass')));
        $driver = new TestFileDriver($locator);

        $classNames = $driver->getAllClassNames();

        $this->assertEquals(array('stdClass'), $classNames);
    }

    public function testGetAllClassNamesBothSources()
    {
        $locator = $this->newLocator();
        $locator->expects($this->any())
                ->method('getAllClassNames')
                ->with($this->equalTo('global'))
                ->will($this->returnValue(array('stdClass')));
        $driver = new TestFileDriver($locator);
        $driver->setGlobalBasename("global");

        $classNames = $driver->getAllClassNames();

        $this->assertEquals(array('stdGlobal', 'stdGlobal2', 'stdClass'), $classNames);
    }

    public function testIsNotTransient()
    {
        $locator = $this->newLocator();
        $locator->expects($this->once())
                ->method('fileExists')
                ->with($this->equalTo('stdClass'))
                ->will($this->returnValue( true ));

        $driver = new TestFileDriver($locator);
        $driver->setGlobalBasename("global");

        $this->assertFalse($driver->isTransient('stdClass'));
        $this->assertFalse($driver->isTransient('stdGlobal'));
        $this->assertFalse($driver->isTransient('stdGlobal2'));
    }

    public function testIsTransient()
    {
        $locator = $this->newLocator();
        $locator->expects($this->once())
                ->method('fileExists')
                ->with($this->equalTo('stdClass2'))
                ->will($this->returnValue( false ));

        $driver = new TestFileDriver($locator);

        $this->assertTrue($driver->isTransient('stdClass2'));
    }

    public function testNonLocatorFallback()
    {
        $driver = new TestFileDriver(__DIR__ . '/_files', '.yml');
        $this->assertTrue($driver->isTransient('stdClass2'));
        $this->assertFalse($driver->isTransient('stdClass'));
    }

    private function newLocator()
    {
        $locator = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\FileLocator');
        $locator->expects($this->any())->method('getFileExtension')->will($this->returnValue('.yml'));
        $locator->expects($this->any())->method('getPaths')->will($this->returnValue(array(__DIR__ . "/_files")));
        return $locator;
    }
}

class TestFileDriver extends FileDriver
{
    protected function loadMappingFile($file)
    {
        if (strpos($file, "global.yml") !== false) {
            return array('stdGlobal' => 'stdGlobal', 'stdGlobal2' => 'stdGlobal2');
        }
        return array('stdClass' => 'stdClass');
    }

    public function loadMetadataForClass($className, ClassMetadata $metadata)
    {

    }
}