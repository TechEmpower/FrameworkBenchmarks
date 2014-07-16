<?php

namespace Doctrine\Tests\Common\Cache;

use Doctrine\Common\Cache\Cache;
use ArrayObject;

abstract class CacheTest extends \Doctrine\Tests\DoctrineTestCase
{
    /**
     * @dataProvider provideCrudValues
     */
    public function testBasicCrudOperations($value)
    {
        $cache = $this->_getCacheDriver();

        // Test saving a value, checking if it exists, and fetching it back
        $this->assertTrue($cache->save('key', 'value'));
        $this->assertTrue($cache->contains('key'));
        $this->assertEquals('value', $cache->fetch('key'));

        // Test updating the value of a cache entry
        $this->assertTrue($cache->save('key', 'value-changed'));
        $this->assertTrue($cache->contains('key'));
        $this->assertEquals('value-changed', $cache->fetch('key'));

        // Test deleting a value
        $this->assertTrue($cache->delete('key'));
        $this->assertFalse($cache->contains('key'));
    }

    public function provideCrudValues()
    {
        return array(
            'array' => array(array('one', 2, 3.0)),
            'string' => array('value'),
            'integer' => array(1),
            'float' => array(1.5),
            'object' => array(new ArrayObject()),
        );
    }

    public function testDeleteAll()
    {
        $cache = $this->_getCacheDriver();

        $this->assertTrue($cache->save('key1', 1));
        $this->assertTrue($cache->save('key2', 2));
        $this->assertTrue($cache->deleteAll());
        $this->assertFalse($cache->contains('key1'));
        $this->assertFalse($cache->contains('key2'));
    }

    public function testDeleteAllAndNamespaceVersioningBetweenCaches()
    {
        if ( ! $this->isSharedStorage()) {
            $this->markTestSkipped('The ' . __CLASS__ .' does not use shared storage');
        }

        $cache1 = $this->_getCacheDriver();
        $cache2 = $this->_getCacheDriver();

        $this->assertTrue($cache1->save('key1', 1));
        $this->assertTrue($cache2->save('key2', 2));

        /* Both providers are initialized with the same namespace version, so
         * they can see entries set by each other.
         */
        $this->assertTrue($cache1->contains('key1'));
        $this->assertTrue($cache1->contains('key2'));
        $this->assertTrue($cache2->contains('key1'));
        $this->assertTrue($cache2->contains('key2'));

        /* Deleting all entries through one provider will only increment the
         * namespace version on that object (and in the cache itself, which new
         * instances will use to initialize). The second provider will retain
         * its original version and still see stale data.
         */
        $this->assertTrue($cache1->deleteAll());
        $this->assertFalse($cache1->contains('key1'));
        $this->assertFalse($cache1->contains('key2'));
        $this->assertTrue($cache2->contains('key1'));
        $this->assertTrue($cache2->contains('key2'));

        /* A new cache provider should not see the deleted entries, since its
         * namespace version will be initialized.
         */
        $cache3 = $this->_getCacheDriver();
        $this->assertFalse($cache3->contains('key1'));
        $this->assertFalse($cache3->contains('key2'));
    }

    public function testFlushAll()
    {
        $cache = $this->_getCacheDriver();

        $this->assertTrue($cache->save('key1', 1));
        $this->assertTrue($cache->save('key2', 2));
        $this->assertTrue($cache->flushAll());
        $this->assertFalse($cache->contains('key1'));
        $this->assertFalse($cache->contains('key2'));
    }

    public function testFlushAllAndNamespaceVersioningBetweenCaches()
    {
        if ( ! $this->isSharedStorage()) {
            $this->markTestSkipped('The ' . __CLASS__ .' does not use shared storage');
        }

        $cache1 = $this->_getCacheDriver();
        $cache2 = $this->_getCacheDriver();

        /* Deleting all elements from the first provider should increment its
         * namespace version before saving the first entry.
         */
        $cache1->deleteAll();
        $this->assertTrue($cache1->save('key1', 1));

        /* The second provider will be initialized with the same namespace
         * version upon its first save operation.
         */
        $this->assertTrue($cache2->save('key2', 2));

        /* Both providers have the same namespace version and can see entires
         * set by each other.
         */
        $this->assertTrue($cache1->contains('key1'));
        $this->assertTrue($cache1->contains('key2'));
        $this->assertTrue($cache2->contains('key1'));
        $this->assertTrue($cache2->contains('key2'));

        /* Flushing all entries through one cache will remove all entries from
         * the cache but leave their namespace version as-is.
         */
        $this->assertTrue($cache1->flushAll());
        $this->assertFalse($cache1->contains('key1'));
        $this->assertFalse($cache1->contains('key2'));
        $this->assertFalse($cache2->contains('key1'));
        $this->assertFalse($cache2->contains('key2'));

        /* Inserting a new entry will use the same, incremented namespace
         * version, and it will be visible to both providers.
         */
        $this->assertTrue($cache1->save('key1', 1));
        $this->assertTrue($cache1->contains('key1'));
        $this->assertTrue($cache2->contains('key1'));

        /* A new cache provider will be initialized with the original namespace
         * version and not share any visibility with the first two providers.
         */
        $cache3 = $this->_getCacheDriver();
        $this->assertFalse($cache3->contains('key1'));
        $this->assertFalse($cache3->contains('key2'));
        $this->assertTrue($cache3->save('key3', 3));
        $this->assertTrue($cache3->contains('key3'));
    }

    public function testNamespace()
    {
        $cache = $this->_getCacheDriver();

        $cache->setNamespace('ns1_');

        $this->assertTrue($cache->save('key1', 1));
        $this->assertTrue($cache->contains('key1'));

        $cache->setNamespace('ns2_');

        $this->assertFalse($cache->contains('key1'));
    }

    public function testDeleteAllNamespace()
    {
        $cache = $this->_getCacheDriver();

        $cache->setNamespace('ns1');
        $this->assertFalse($cache->contains('key1'));
        $cache->save('key1', 'test');
        $this->assertTrue($cache->contains('key1'));

        $cache->setNamespace('ns2');
        $this->assertFalse($cache->contains('key1'));
        $cache->save('key1', 'test');
        $this->assertTrue($cache->contains('key1'));

        $cache->setNamespace('ns1');
        $this->assertTrue($cache->contains('key1'));
        $cache->deleteAll();
        $this->assertFalse($cache->contains('key1'));

        $cache->setNamespace('ns2');
        $this->assertTrue($cache->contains('key1'));
        $cache->deleteAll();
        $this->assertFalse($cache->contains('key1'));
    }

    /**
     * @group DCOM-43
     */
    public function testGetStats()
    {
        $cache = $this->_getCacheDriver();
        $stats = $cache->getStats();

        $this->assertArrayHasKey(Cache::STATS_HITS, $stats);
        $this->assertArrayHasKey(Cache::STATS_MISSES, $stats);
        $this->assertArrayHasKey(Cache::STATS_UPTIME, $stats);
        $this->assertArrayHasKey(Cache::STATS_MEMORY_USAGE, $stats);
        $this->assertArrayHasKey(Cache::STATS_MEMORY_AVAILABLE, $stats);
    }

    public function testFetchMissShouldReturnFalse()
    {
        $cache = $this->_getCacheDriver();

        /* Ensure that caches return boolean false instead of null on a fetch
         * miss to be compatible with ORM integration.
         */
        $result = $cache->fetch('nonexistent_key');

        $this->assertFalse($result);
        $this->assertNotNull($result);
    }

    /**
     * Return whether multiple cache providers share the same storage.
     *
     * This is used for skipping certain tests for shared storage behavior.
     *
     * @return boolean
     */
    protected function isSharedStorage()
    {
        return true;
    }

    /**
     * @return \Doctrine\Common\Cache\CacheProvider
     */
    abstract protected function _getCacheDriver();
}
