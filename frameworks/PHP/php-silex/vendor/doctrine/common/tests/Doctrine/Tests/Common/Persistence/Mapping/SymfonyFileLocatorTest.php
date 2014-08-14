<?php

namespace Doctrine\Tests\Common\Persistence\Mapping;

use Doctrine\Tests\DoctrineTestCase;
use Doctrine\Common\Persistence\Mapping\Driver\SymfonyFileLocator;

class SymfonyFileLocatorTest extends DoctrineTestCase
{
    public function testGetPaths()
    {
        $path = __DIR__ . "/_files";
        $prefix = "Foo";

        $locator = new SymfonyFileLocator(array($path => $prefix));
        $this->assertEquals(array($path), $locator->getPaths());

        $locator = new SymfonyFileLocator(array($path => $prefix));
        $this->assertEquals(array($path), $locator->getPaths());
    }

    public function testGetPrefixes()
    {
        $path = __DIR__ . "/_files";
        $prefix = "Foo";

        $locator = new SymfonyFileLocator(array($path => $prefix));
        $this->assertEquals(array($path => $prefix), $locator->getNamespacePrefixes());
    }

    public function testGetFileExtension()
    {
        $locator = new SymfonyFileLocator(array(), ".yml");
        $this->assertEquals(".yml", $locator->getFileExtension());
        $locator->setFileExtension(".xml");
        $this->assertEquals(".xml", $locator->getFileExtension());
    }

    public function testFileExists()
    {
        $path = __DIR__ . "/_files";
        $prefix = "Foo";

        $locator = new SymfonyFileLocator(array($path => $prefix), ".yml");

        $this->assertTrue($locator->fileExists("Foo\stdClass"));
        $this->assertTrue($locator->fileExists("Foo\global"));
        $this->assertFalse($locator->fileExists("Foo\stdClass2"));
        $this->assertFalse($locator->fileExists("Foo\global2"));
    }

    public function testGetAllClassNames()
    {
        $path = __DIR__ . "/_files";
        $prefix = "Foo";

        $locator = new SymfonyFileLocator(array($path => $prefix), ".yml");
        $classes = $locator->getAllClassNames(null);
        sort($classes);

        $this->assertEquals(array("Foo\\global", "Foo\\stdClass"), $classes);
        $this->assertEquals(array("Foo\\stdClass"), $locator->getAllClassNames("global"));
    }

    public function testFindMappingFile()
    {
        $path = __DIR__ . "/_files";
        $prefix = "Foo";

        $locator = new SymfonyFileLocator(array($path => $prefix), ".yml");

        $this->assertEquals(__DIR__ . "/_files/stdClass.yml", $locator->findMappingFile("Foo\\stdClass"));
    }

    public function testFindMappingFileNotFound()
    {
        $path = __DIR__ . "/_files";
        $prefix = "Foo";

        $locator = new SymfonyFileLocator(array($path => $prefix), ".yml");

        $this->setExpectedException(
            "Doctrine\Common\Persistence\Mapping\MappingException",
            "No mapping file found named '".__DIR__."/_files/stdClass2.yml' for class 'Foo\stdClass2'."
        );
        $locator->findMappingFile("Foo\\stdClass2");
    }
}
