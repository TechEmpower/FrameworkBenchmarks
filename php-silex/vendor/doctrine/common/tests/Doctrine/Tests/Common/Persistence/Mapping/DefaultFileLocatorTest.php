<?php

namespace Doctrine\Tests\Common\Persistence\Mapping;

use Doctrine\Tests\DoctrineTestCase;
use Doctrine\Common\Persistence\Mapping\Driver\DefaultFileLocator;

class DefaultFileLocatorTest extends DoctrineTestCase
{
    public function testGetPaths()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path));
        $this->assertEquals(array($path), $locator->getPaths());

        $locator = new DefaultFileLocator($path);
        $this->assertEquals(array($path), $locator->getPaths());
    }

    public function testGetFileExtension()
    {
        $locator = new DefaultFileLocator(array(), ".yml");
        $this->assertEquals(".yml", $locator->getFileExtension());
        $locator->setFileExtension(".xml");
        $this->assertEquals(".xml", $locator->getFileExtension());
    }

    public function testUniquePaths()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path, $path));
        $this->assertEquals(array($path), $locator->getPaths());
    }

    public function testFindMappingFile()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path), ".yml");

        $this->assertEquals(__DIR__ . '/_files' . DIRECTORY_SEPARATOR . 'stdClass.yml', $locator->findMappingFile('stdClass'));
    }

    public function testFindMappingFileNotFound()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path), ".yml");

        $this->setExpectedException(
            'Doctrine\Common\Persistence\Mapping\MappingException',
            "No mapping file found named 'stdClass2.yml' for class 'stdClass2'"
        );
        $locator->findMappingFile('stdClass2');
    }

    public function testGetAllClassNames()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path), ".yml");
        $classes = $locator->getAllClassNames(null);
        sort($classes);

        $this->assertEquals(array('global', 'stdClass'), $classes);
        $this->assertEquals(array('stdClass'), $locator->getAllClassNames("global"));
    }

    public function testGetAllClassNamesNonMatchingFileExtension()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path), ".xml");
        $this->assertEquals(array(), $locator->getAllClassNames("global"));
    }

    public function testFileExists()
    {
        $path = __DIR__ . "/_files";

        $locator = new DefaultFileLocator(array($path), ".yml");

        $this->assertTrue($locator->fileExists("stdClass"));
        $this->assertFalse($locator->fileExists("stdClass2"));
        $this->assertTrue($locator->fileExists("global"));
        $this->assertFalse($locator->fileExists("global2"));
    }
}
