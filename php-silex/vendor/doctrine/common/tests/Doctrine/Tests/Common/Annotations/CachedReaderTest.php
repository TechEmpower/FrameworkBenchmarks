<?php

namespace Doctrine\Tests\Common\Annotations;

use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Route;
use Doctrine\Common\Annotations\AnnotationReader;
use Doctrine\Common\Annotations\CachedReader;
use Doctrine\Common\Cache\ArrayCache;

class CachedReaderTest extends AbstractReaderTest
{
    private $cache;

    public function testIgnoresStaleCache()
    {
        $file = __DIR__.'/Fixtures/Controller.php';
        touch($file);
        $name = 'Doctrine\Tests\Common\Annotations\Fixtures\Controller';
        $cacheKey = $name.'@[Annot]';

        $cache = $this->getMock('Doctrine\Common\Cache\Cache');
        $cache
            ->expects($this->at(0))
            ->method('fetch')
            ->with($this->equalTo($cacheKey))
            ->will($this->returnValue(array()))
        ;
        $cache
            ->expects($this->at(1))
            ->method('fetch')
            ->with($this->equalTo('[C]'.$cacheKey))
            ->will($this->returnValue(time() - 10))
        ;
        $cache
            ->expects($this->at(2))
            ->method('save')
            ->with($this->equalTo($cacheKey))
        ;
        $cache
            ->expects($this->at(3))
            ->method('save')
            ->with($this->equalTo('[C]'.$cacheKey))
        ;

        $reader = new CachedReader(new AnnotationReader(), $cache, true);
        $route = new Route();
        $route->pattern = '/someprefix';
        $this->assertEquals(array($route), $reader->getClassAnnotations(new \ReflectionClass($name)));
    }

    protected function getReader()
    {
        $this->cache = new ArrayCache();
        return new CachedReader(new AnnotationReader(), $this->cache);
    }
}