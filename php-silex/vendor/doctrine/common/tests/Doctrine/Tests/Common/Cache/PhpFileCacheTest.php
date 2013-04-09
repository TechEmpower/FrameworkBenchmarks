<?php

namespace Doctrine\Tests\Common\Cache;

use Doctrine\Common\Cache\PhpFileCache;

/**
 * @group DCOM-101
 */
class PhpFileCacheTest extends CacheTest
{
    /**
     * @var \Doctrine\Common\Cache\PhpFileCache
     */
    private $driver;

    protected function _getCacheDriver()
    {
        $dir = sys_get_temp_dir() . "/doctrine_cache_". uniqid();
        $this->assertFalse(is_dir($dir));

        $this->driver = new PhpFileCache($dir);
        $this->assertTrue(is_dir($dir));

        return $this->driver;
    }

    public function testObjects()
    {
        $this->markTestSkipped('PhpFileCache does not support saving objects that dont implement __set_state()');
    }

    public function testLifetime()
    {
        $cache = $this->_getCacheDriver();

        // Test save
        $cache->save('test_key', 'testing this out', 10);

        // Test contains to test that save() worked
        $this->assertTrue($cache->contains('test_key'));

        // Test fetch
        $this->assertEquals('testing this out', $cache->fetch('test_key'));

        // access private methods
        $getFilename        = new \ReflectionMethod($cache, 'getFilename');
        $getNamespacedId    = new \ReflectionMethod($cache, 'getNamespacedId');

        $getFilename->setAccessible(true);
        $getNamespacedId->setAccessible(true);

        $id     = $getNamespacedId->invoke($cache, 'test_key');
        $path   = $getFilename->invoke($cache, $id);
        $value  = include $path;

        // update lifetime
        $value['lifetime'] = $value['lifetime'] - 20;
        file_put_contents($path, '<?php return unserialize(' . var_export(serialize($value), true) . ');');

        // test expired data
        $this->assertFalse($cache->contains('test_key'));
        $this->assertFalse($cache->fetch('test_key'));
    }

    public function testImplementsSetState()
    {
        $cache = $this->_getCacheDriver();

        // Test save
        $cache->save('test_set_state', new SetStateClass(array(1,2,3)));

        //Test __set_state call
        $this->assertCount(0, SetStateClass::$values);

        // Test fetch
        $value = $cache->fetch('test_set_state');
        $this->assertInstanceOf('Doctrine\Tests\Common\Cache\SetStateClass', $value);
        $this->assertEquals(array(1,2,3), $value->getValue());

        //Test __set_state call
        $this->assertCount(1, SetStateClass::$values);

        // Test contains
        $this->assertTrue($cache->contains('test_set_state'));
    }

    public function testNotImplementsSetState()
    {
        $cache = $this->_getCacheDriver();

        $this->setExpectedException('InvalidArgumentException');
        $cache->save('test_not_set_state', new NotSetStateClass(array(1,2,3)));
    }

    public function testGetStats()
    {
        $cache = $this->_getCacheDriver();
        $stats = $cache->getStats();

        $this->assertNull($stats);
    }

    public function tearDown()
    {
        if (!$this->driver) {
            return;
        }

        $dir        = $this->driver->getDirectory();
        $ext        = $this->driver->getExtension();
        $iterator   = new \RecursiveDirectoryIterator($dir);

        foreach (new \RecursiveIteratorIterator($iterator, \RecursiveIteratorIterator::CHILD_FIRST) as $file) {
            if ($file->isFile()) {
                @unlink($file->getRealPath());
            } else {
                @rmdir($file->getRealPath());
            }
        }
    }

}

class NotSetStateClass
{
    private $value;

    public function __construct($value)
    {
        $this->value = $value;
    }

    public function getValue()
    {
        return $this->value;
    }
}

class SetStateClass extends NotSetStateClass
{
    public static $values = array();

    public static function __set_state($data)
    {
        self::$values = $data;
        return new self($data['value']);
    }
}
