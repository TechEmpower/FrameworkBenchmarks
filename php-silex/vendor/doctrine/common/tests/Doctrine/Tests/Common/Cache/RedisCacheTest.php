<?php

namespace Doctrine\Tests\Common\Cache;

use Doctrine\Common\Cache\RedisCache;

class RedisCacheTest extends CacheTest
{
    private $_redis;

    public function setUp()
    {
        if (extension_loaded('redis')) {
            $this->_redis = new \Redis();
            $ok = @$this->_redis->connect('127.0.0.1');
            if (!$ok) {
                $this->markTestSkipped('The ' . __CLASS__ .' requires the use of redis');
            }
        } else {
            $this->markTestSkipped('The ' . __CLASS__ .' requires the use of redis');
        }
    }

    protected function _getCacheDriver()
    {
        $driver = new RedisCache();
        $driver->setRedis($this->_redis);
        return $driver;
    }
}
