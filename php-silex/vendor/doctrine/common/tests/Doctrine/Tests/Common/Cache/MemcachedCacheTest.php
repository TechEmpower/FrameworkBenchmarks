<?php

namespace Doctrine\Tests\Common\Cache;

use Doctrine\Common\Cache\MemcachedCache;

class MemcachedCacheTest extends CacheTest
{
    private $memcached;

    public function setUp()
    {
        if (extension_loaded('memcached')) {
            $this->memcached = new \Memcached();
            $this->memcached->setOption(\Memcached::OPT_COMPRESSION, false);
            $this->memcached->addServer('127.0.0.1', 11211);

            $fh = @fsockopen('127.0.0.1', 11211);
            if (!$fh) {
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

        $this->assertTrue($cache->contains('key'), 'Memcached provider should support TTL > 30 days');
    }

    protected function _getCacheDriver()
    {
        $driver = new MemcachedCache();
        $driver->setMemcached($this->memcached);
        return $driver;
    }
}
