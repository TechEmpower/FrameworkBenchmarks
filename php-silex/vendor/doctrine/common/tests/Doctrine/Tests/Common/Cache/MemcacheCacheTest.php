<?php

namespace Doctrine\Tests\Common\Cache;

use Doctrine\Common\Cache\MemcacheCache;

class MemcacheCacheTest extends CacheTest
{
    private $_memcache;

    public function setUp()
    {
        if (extension_loaded('memcache')) {
            $this->_memcache = new \Memcache;
            $ok = @$this->_memcache->connect('localhost', 11211);
            if (!$ok) {
                $this->markTestSkipped('The ' . __CLASS__ .' requires the use of memcache');
            }
        } else {
            $this->markTestSkipped('The ' . __CLASS__ .' requires the use of memcache');
        }
    }

    public function testNoExpire() {
        $cache = $this->_getCacheDriver();
        $cache->save('noexpire', 'value', 0);
        sleep(1);
        $this->assertTrue($cache->contains('noexpire'), 'Memcache provider should support no-expire');
    }

    public function testLongLifetime()
    {
        $cache = $this->_getCacheDriver();
        $cache->save('key', 'value', 30 * 24 * 3600 + 1);
        $this->assertTrue($cache->contains('key'), 'Memcache provider should support TTL > 30 days');
    }

    protected function _getCacheDriver()
    {
        $driver = new MemcacheCache();
        $driver->setMemcache($this->_memcache);
        return $driver;
    }

}
