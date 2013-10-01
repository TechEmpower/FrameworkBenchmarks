<?php

namespace Doctrine\Tests\Common\Collections;

use Doctrine\Tests;
use Doctrine\Common\Collections\Criteria;

class CollectionTest extends \Doctrine\Tests\DoctrineTestCase
{
    /**
     * @var \Doctrine\Common\Collections\Collection
     */
    private $_coll;

    protected function setUp()
    {
        $this->_coll = new \Doctrine\Common\Collections\ArrayCollection;
    }

    public function testIssetAndUnset()
    {
        $this->assertFalse(isset($this->_coll[0]));
        $this->_coll->add('testing');
        $this->assertTrue(isset($this->_coll[0]));
        unset($this->_coll[0]);
        $this->assertFalse(isset($this->_coll[0]));
    }

    public function testToString()
    {
        $this->_coll->add('testing');
        $this->assertTrue(is_string((string) $this->_coll));
    }

    public function testRemovingNonExistentEntryReturnsNull()
    {
        $this->assertEquals(null, $this->_coll->remove('testing_does_not_exist'));
    }

    public function testExists()
    {
        $this->_coll->add("one");
        $this->_coll->add("two");
        $exists = $this->_coll->exists(function($k, $e) { return $e == "one"; });
        $this->assertTrue($exists);
        $exists = $this->_coll->exists(function($k, $e) { return $e == "other"; });
        $this->assertFalse($exists);
    }

    public function testMap()
    {
        $this->_coll->add(1);
        $this->_coll->add(2);
        $res = $this->_coll->map(function($e) { return $e * 2; });
        $this->assertEquals(array(2, 4), $res->toArray());
    }

    public function testFilter()
    {
        $this->_coll->add(1);
        $this->_coll->add("foo");
        $this->_coll->add(3);
        $res = $this->_coll->filter(function($e) { return is_numeric($e); });
        $this->assertEquals(array(0 => 1, 2 => 3), $res->toArray());
    }

    public function testFirstAndLast()
    {
        $this->_coll->add('one');
        $this->_coll->add('two');

        $this->assertEquals($this->_coll->first(), 'one');
        $this->assertEquals($this->_coll->last(), 'two');
    }

    public function testArrayAccess()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';

        $this->assertEquals($this->_coll[0], 'one');
        $this->assertEquals($this->_coll[1], 'two');

        unset($this->_coll[0]);
        $this->assertEquals($this->_coll->count(), 1);
    }

    public function testContainsKey()
    {
        $this->_coll[5] = 'five';
        $this->assertTrue($this->_coll->containsKey(5));
    }

    public function testContains()
    {
        $this->_coll[0] = 'test';
        $this->assertTrue($this->_coll->contains('test'));
    }

    public function testSearch()
    {
        $this->_coll[0] = 'test';
        $this->assertEquals(0, $this->_coll->indexOf('test'));
    }

    public function testGet()
    {
        $this->_coll[0] = 'test';
        $this->assertEquals('test', $this->_coll->get(0));
    }

    public function testGetKeys()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $this->assertEquals(array(0, 1), $this->_coll->getKeys());
    }

    public function testGetValues()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $this->assertEquals(array('one', 'two'), $this->_coll->getValues());
    }

    public function testCount()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $this->assertEquals($this->_coll->count(), 2);
        $this->assertEquals(count($this->_coll), 2);
    }

    public function testForAll()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $this->assertEquals($this->_coll->forAll(function($k, $e) { return is_string($e); }), true);
        $this->assertEquals($this->_coll->forAll(function($k, $e) { return is_array($e); }), false);
    }

    public function testPartition()
    {
        $this->_coll[] = true;
        $this->_coll[] = false;
        $partition = $this->_coll->partition(function($k, $e) { return $e == true; });
        $this->assertEquals($partition[0][0], true);
        $this->assertEquals($partition[1][0], false);
    }

    public function testClear()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $this->_coll->clear();
        $this->assertEquals($this->_coll->isEmpty(), true);
    }

    public function testRemove()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $el = $this->_coll->remove(0);

        $this->assertEquals('one', $el);
        $this->assertEquals($this->_coll->contains('one'), false);
        $this->assertNull($this->_coll->remove(0));
    }

    public function testRemoveElement()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';

        $this->assertTrue($this->_coll->removeElement('two'));
        $this->assertFalse($this->_coll->contains('two'));
        $this->assertFalse($this->_coll->removeElement('two'));
    }

    public function testSlice()
    {
        $this->_coll[] = 'one';
        $this->_coll[] = 'two';
        $this->_coll[] = 'three';

        $slice = $this->_coll->slice(0, 1);
        $this->assertInternalType('array', $slice);
        $this->assertEquals(array('one'), $slice);

        $slice = $this->_coll->slice(1);
        $this->assertEquals(array(1 => 'two', 2 => 'three'), $slice);

        $slice = $this->_coll->slice(1, 1);
        $this->assertEquals(array(1 => 'two'), $slice);
    }

    public function fillMatchingFixture()
    {
        $std1 = new \stdClass();
        $std1->foo = "bar";
        $this->_coll[] = $std1;

        $std2 = new \stdClass();
        $std2->foo = "baz";
        $this->_coll[] = $std2;
    }

    /**
     * @group DDC-1637
     */
    public function testMatching()
    {
        $this->fillMatchingFixture();

        $col = $this->_coll->matching(new Criteria(Criteria::expr()->eq("foo", "bar")));
        $this->assertInstanceOf('Doctrine\Common\Collections\Collection', $col);
        $this->assertNotSame($col, $this->_coll);
        $this->assertEquals(1, count($col));
    }

    /**
     * @group DDC-1637
     */
    public function testMatchingOrdering()
    {
        $this->fillMatchingFixture();

        $col = $this->_coll->matching(new Criteria(null, array('foo' => 'DESC')));

        $this->assertInstanceOf('Doctrine\Common\Collections\Collection', $col);
        $this->assertNotSame($col, $this->_coll);
        $this->assertEquals(2, count($col));
        $this->assertEquals('baz', $col[0]->foo);
        $this->assertEquals('bar', $col[1]->foo);
    }

    /**
     * @group DDC-1637
     */
    public function testMatchingSlice()
    {
        $this->fillMatchingFixture();

        $col = $this->_coll->matching(new Criteria(null, null, 1, 1));

        $this->assertInstanceOf('Doctrine\Common\Collections\Collection', $col);
        $this->assertNotSame($col, $this->_coll);
        $this->assertEquals(1, count($col));
        $this->assertEquals('baz', $col[0]->foo);
    }

    public function testCanRemoveNullValuesByKey()
    {
        $this->_coll->add(null);
        $this->_coll->remove(0);
        $this->assertTrue($this->_coll->isEmpty());
    }

    public function testCanVerifyExistingKeysWithNullValues()
    {
        $this->_coll->set('key', null);
        $this->assertTrue($this->_coll->containsKey('key'));
    }
}
