<?php

namespace Doctrine\Tests\Common\Persistence;

use Doctrine\Common\Persistence\PersistentObject;
use Doctrine\Common\Persistence\Mapping\ClassMetadata;
use Doctrine\Common\Persistence\Mapping\ReflectionService;

/**
 * @group DDC-1448
 */
class PersistentObjectTest extends \Doctrine\Tests\DoctrineTestCase
{
    private $cm;
    private $om;
    private $object;

    public function setUp()
    {
        $this->cm = new TestObjectMetadata;
        $this->om = $this->getMock('Doctrine\Common\Persistence\ObjectManager');
        $this->om->expects($this->any())->method('getClassMetadata')
                 ->will($this->returnValue($this->cm));
        $this->object = new TestObject;
        PersistentObject::setObjectManager($this->om);
        $this->object->injectObjectManager($this->om, $this->cm);
    }

    public function testGetObjectManager()
    {
        $this->assertSame($this->om, PersistentObject::getObjectManager());
    }

    public function testNonMatchingObjectManager()
    {
        $this->setExpectedException('RuntimeException');
        $om = $this->getMock('Doctrine\Common\Persistence\ObjectManager');
        $this->object->injectObjectManager($om, $this->cm);
    }

    public function testGetField()
    {
        $this->assertEquals('beberlei', $this->object->getName());
    }

    public function testSetField()
    {
        $this->object->setName("test");
        $this->assertEquals("test", $this->object->getName());
    }

    public function testGetIdentifier()
    {
        $this->assertEquals(1, $this->object->getId());
    }

    public function testSetIdentifier()
    {
        $this->setExpectedException('BadMethodCallException');
        $this->object->setId(2);
    }

    public function testSetUnknownField()
    {
        $this->setExpectedException('BadMethodCallException');
        $this->object->setUnknown("test");
    }

    public function testGetUnknownField()
    {
        $this->setExpectedException('BadMethodCallException');
        $this->object->getUnknown();
    }

    public function testGetToOneAssociation()
    {
        $this->assertNull($this->object->getParent());
    }

    public function testSetToOneAssociation()
    {
        $parent = new TestObject();
        $this->object->setParent($parent);
        $this->assertSame($parent, $this->object->getParent($parent));
    }

    public function testSetInvalidToOneAssociation()
    {
        $parent = new \stdClass();

        $this->setExpectedException('InvalidArgumentException');
        $this->object->setParent($parent);
    }

    public function testSetToOneAssociationNull()
    {
        $parent = new TestObject();
        $this->object->setParent($parent);
        $this->object->setParent(null);
        $this->assertNull($this->object->getParent());
    }

    public function testAddToManyAssociation()
    {
        $child = new TestObject();
        $this->object->addChildren($child);

        $this->assertSame($this->object, $child->getParent());
        $this->assertEquals(1, count($this->object->getChildren()));

        $child = new TestObject();
        $this->object->addChildren($child);

        $this->assertEquals(2, count($this->object->getChildren()));
    }

    public function testAddInvalidToManyAssociation()
    {
        $this->setExpectedException('InvalidArgumentException');
        $this->object->addChildren(new \stdClass());
    }

    public function testNoObjectManagerSet()
    {
        PersistentObject::setObjectManager(null);
        $child = new TestObject();

        $this->setExpectedException('RuntimeException');
        $child->setName("test");
    }

    public function testInvalidMethod()
    {
        $this->setExpectedException('BadMethodCallException');
        $this->object->asdf();
    }

    public function testAddInvalidCollection()
    {
        $this->setExpectedException('BadMethodCallException');
        $this->object->addAsdf(new \stdClass());
    }
}

class TestObject extends PersistentObject
{
    protected $id = 1;
    protected $name = 'beberlei';
    protected $parent;
    protected $children;
}

class TestObjectMetadata implements ClassMetadata
{

    public function getAssociationMappedByTargetField($assocName)
    {
        $assoc = array('children' => 'parent');
        return $assoc[$assocName];
    }

    public function getAssociationNames()
    {
        return array('parent', 'children');
    }

    public function getAssociationTargetClass($assocName)
    {
        return __NAMESPACE__ . '\TestObject';
    }

    public function getFieldNames()
    {
        return array('id', 'name');
    }

    public function getIdentifier()
    {
        return array('id');
    }

    public function getName()
    {
        return __NAMESPACE__ . '\TestObject';
    }

    public function getReflectionClass()
    {
        return new \ReflectionClass($this->getName());
    }

    public function getTypeOfField($fieldName)
    {
        $types = array('id' => 'integer', 'name' => 'string');
        return $types[$fieldName];
    }

    public function hasAssociation($fieldName)
    {
        return in_array($fieldName, array('parent', 'children'));
    }

    public function hasField($fieldName)
    {
        return in_array($fieldName, array('id', 'name'));
    }

    public function isAssociationInverseSide($assocName)
    {
        return ($assocName === 'children');
    }

    public function isCollectionValuedAssociation($fieldName)
    {
        return ($fieldName === 'children');
    }

    public function isIdentifier($fieldName)
    {
        return $fieldName === 'id';
    }

    public function isSingleValuedAssociation($fieldName)
    {
        return $fieldName === 'parent';
    }

    public function getIdentifierValues($entity)
    {

    }

    public function getIdentifierFieldNames()
    {

    }

    public function initializeReflection(ReflectionService $reflService)
    {

    }

    public function wakeupReflection(ReflectionService $reflService)
    {

    }
}
