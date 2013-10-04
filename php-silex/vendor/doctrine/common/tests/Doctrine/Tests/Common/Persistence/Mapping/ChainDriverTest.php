<?php

namespace Doctrine\Tests\Common\Persistence\Mapping;

use Doctrine\Common\Persistence\Mapping\Driver\MappingDriverChain;
use Doctrine\Tests\DoctrineTestCase;

class DriverChainTest extends DoctrineTestCase
{
    public function testDelegateToMatchingNamespaceDriver()
    {
        $className = 'Doctrine\Tests\Common\Persistence\Mapping\DriverChainEntity';
        $classMetadata = $this->getMock('Doctrine\Common\Persistence\Mapping\ClassMetadata');

        $chain = new MappingDriverChain();

        $driver1 = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $driver1->expects($this->never())
                ->method('loadMetadataForClass');
        $driver1->expectS($this->never())
                ->method('isTransient');

        $driver2 = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $driver2->expects($this->at(0))
                ->method('loadMetadataForClass')
                ->with($this->equalTo($className), $this->equalTo($classMetadata));
        $driver2->expects($this->at(1))
                ->method('isTransient')
                ->with($this->equalTo($className))
                ->will($this->returnValue( true ));

        $chain->addDriver($driver1, 'Doctrine\Tests\Models\Company');
        $chain->addDriver($driver2, 'Doctrine\Tests\Common\Persistence\Mapping');

        $chain->loadMetadataForClass($className, $classMetadata);

        $this->assertTrue( $chain->isTransient($className) );
    }

    public function testLoadMetadata_NoDelegatorFound_ThrowsMappingException()
    {
        $className = 'Doctrine\Tests\Common\Persistence\Mapping\DriverChainEntity';
        $classMetadata = $this->getMock('Doctrine\Common\Persistence\Mapping\ClassMetadata');

        $chain = new MappingDriverChain();

        $this->setExpectedException('Doctrine\Common\Persistence\Mapping\MappingException');
        $chain->loadMetadataForClass($className, $classMetadata);
    }

    public function testGatherAllClassNames()
    {
        $className = 'Doctrine\Tests\Common\Persistence\Mapping\DriverChainEntity';
        $classMetadata = $this->getMock('Doctrine\Common\Persistence\ClassMetadata');

        $chain = new MappingDriverChain();

        $driver1 = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $driver1->expects($this->once())
                ->method('getAllClassNames')
                ->will($this->returnValue(array('Doctrine\Tests\Models\Company\Foo')));

        $driver2 = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $driver2->expects($this->once())
                ->method('getAllClassNames')
                ->will($this->returnValue(array('Doctrine\Tests\ORM\Mapping\Bar', 'Doctrine\Tests\ORM\Mapping\Baz', 'FooBarBaz')));

        $chain->addDriver($driver1, 'Doctrine\Tests\Models\Company');
        $chain->addDriver($driver2, 'Doctrine\Tests\ORM\Mapping');

        $this->assertEquals(array(
            'Doctrine\Tests\Models\Company\Foo',
            'Doctrine\Tests\ORM\Mapping\Bar',
            'Doctrine\Tests\ORM\Mapping\Baz'
        ), $chain->getAllClassNames());
    }

    /**
     * @group DDC-706
     */
    public function testIsTransient()
    {
        $driver1 = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $chain = new MappingDriverChain();
        $chain->addDriver($driver1, 'Doctrine\Tests\Models\CMS');

        $this->assertTrue($chain->isTransient('stdClass'), "stdClass isTransient");
    }

    /**
     * @group DDC-1412
     */
    public function testDefaultDriver()
    {
        $companyDriver      = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $defaultDriver      = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $entityClassName    = 'Doctrine\Tests\ORM\Mapping\DriverChainEntity';
        $managerClassName   = 'Doctrine\Tests\Models\Company\CompanyManager';
        $chain              = new MappingDriverChain();

        $companyDriver->expects($this->never())
            ->method('loadMetadataForClass');
        $companyDriver->expects($this->once())
            ->method('isTransient')
            ->with($this->equalTo($managerClassName))
            ->will($this->returnValue(false));

        $defaultDriver->expects($this->never())
            ->method('loadMetadataForClass');
        $defaultDriver->expects($this->once())
            ->method('isTransient')
            ->with($this->equalTo($entityClassName))
            ->will($this->returnValue(true));

        $this->assertNull($chain->getDefaultDriver());

        $chain->setDefaultDriver($defaultDriver);
        $chain->addDriver($companyDriver, 'Doctrine\Tests\Models\Company');

        $this->assertSame($defaultDriver, $chain->getDefaultDriver());

        $this->assertTrue($chain->isTransient($entityClassName));
        $this->assertFalse($chain->isTransient($managerClassName));
    }

    public function testDefaultDriverGetAllClassNames()
    {
        $companyDriver = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $defaultDriver = $this->getMock('Doctrine\Common\Persistence\Mapping\Driver\MappingDriver');
        $chain         = new MappingDriverChain();

        $companyDriver->expects($this->once())
            ->method('getAllClassNames')
            ->will($this->returnValue(array('Doctrine\Tests\Models\Company\Foo')));

        $defaultDriver->expects($this->once())
            ->method('getAllClassNames')
            ->will($this->returnValue(array('Other\Class')));

        $chain->setDefaultDriver($defaultDriver);
        $chain->addDriver($companyDriver, 'Doctrine\Tests\Models\Company');

        $classNames = $chain->getAllClassNames();

        $this->assertEquals(array('Doctrine\Tests\Models\Company\Foo', 'Other\Class'), $classNames);
    }
}

class DriverChainEntity
{

}
