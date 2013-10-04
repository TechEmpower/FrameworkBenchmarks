<?php

namespace Doctrine\Tests\Common\Cache;

/**
 * @group DCOM-101
 */
class FileCacheTest extends \Doctrine\Tests\DoctrineTestCase
{
    /**
     * @var \Doctrine\Common\Cache\FileCache
     */
    private $driver;

    protected function setUp()
    {
        $this->driver = $this->getMock(
            'Doctrine\Common\Cache\FileCache',
            array('doFetch', 'doContains', 'doSave'),
            array(), '', false
        );
    }

    public function getProviderFileName()
    {
         return array(
            //The characters :\/<>"*?| are not valid in Windows filenames.
            array('key:1', 'key1'),
            array('key\2', 'key2'),
            array('key/3', 'key3'),
            array('key<4', 'key4'),
            array('key>5', 'key5'),
            array('key"6', 'key6'),
            array('key*7', 'key7'),
            array('key?8', 'key8'),
            array('key|9', 'key9'),
            array('key[0]','key[0]'),
        );
    }

    /**
     * @dataProvider getProviderFileName
     */
    public function testInvalidFilename($key, $expected)
    {
        $cache  = $this->driver;
        $method = new \ReflectionMethod($cache, 'getFilename');

        $method->setAccessible(true);

        $value  = $method->invoke($cache, $key);
        $actual = pathinfo($value, PATHINFO_FILENAME);

        $this->assertEquals($expected, $actual);
    }

    public function testFilenameCollision()
    {
        $data['key:0']  = 'key0';
        $data['key\0']  = 'key0';
        $data['key/0']  = 'key0';
        $data['key<0']  = 'key0';
        $data['key>0']  = 'key0';
        $data['key"0']  = 'key0';
        $data['key*0']  = 'key0';
        $data['key?0']  = 'key0';
        $data['key|0']  = 'key0';

        $paths  = array();
        $cache  = $this->driver;
        $method = new \ReflectionMethod($cache, 'getFilename');

        $method->setAccessible(true);

        foreach ($data as $key => $expected) {
            $path   = $method->invoke($cache, $key);
            $actual = pathinfo($path, PATHINFO_FILENAME);

            $this->assertNotContains($path, $paths);
            $this->assertEquals($expected, $actual);

            $paths[] = $path;
        }
    }

    public function testFilenameShouldCreateThePathWithFourSubDirectories()
    {
        $cache          = $this->driver;
        $method         = new \ReflectionMethod($cache, 'getFilename');
        $key            = 'item-key';
        $expectedDir[]  = '84e0e2e893febb73';
        $expectedDir[]  = '7a0fee0c89d53f4b';
        $expectedDir[]  = 'b7fcb44c57cdf3d3';
        $expectedDir[]  = '2ce7363f5d597760';
        $expectedDir    = implode(DIRECTORY_SEPARATOR, $expectedDir);

        $method->setAccessible(true);

        $path       = $method->invoke($cache, $key);
        $filename   = pathinfo($path, PATHINFO_FILENAME);
        $dirname    = pathinfo($path, PATHINFO_DIRNAME);

        $this->assertEquals('item-key', $filename);
        $this->assertEquals(DIRECTORY_SEPARATOR . $expectedDir, $dirname);
        $this->assertEquals(DIRECTORY_SEPARATOR . $expectedDir . DIRECTORY_SEPARATOR . $key, $path);
    }
}