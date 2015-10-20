<?php

namespace Doctrine\Tests\Common\Persistence\Mapping;

use Doctrine\Tests\DoctrineTestCase;
use Doctrine\Common\Persistence\Mapping\Driver\StaticPHPDriver;

class StaticPHPDriverTest extends DoctrineTestCase
{
    public function testLoadMetadata()
    {
        $metadata = $this->getMock('Doctrine\Common\Persistence\Mapping\ClassMetadata');
        $metadata->expects($this->once())->method('getFieldNames');

        $driver = new StaticPHPDriver(array(__DIR__));
        $driver->loadMetadataForClass(__NAMESPACE__ . '\\TestEntity', $metadata);
    }

    public function testGetAllClassNames()
    {
        $driver = new StaticPHPDriver(array(__DIR__));
        $classNames = $driver->getAllClassNames();

        $this->assertContains(
            'Doctrine\Tests\Common\Persistence\Mapping\TestEntity', $classNames);
    }
}

class TestEntity
{
    static public function loadMetadata($metadata)
    {
        $metadata->getFieldNames();
    }
}