<?php

namespace Doctrine\Tests\Common\Cache;

use RecursiveDirectoryIterator;
use RecursiveIteratorIterator;

abstract class BaseFileCacheTest extends CacheTest
{
    protected $directory;

    public function setUp()
    {
        do {
            $this->directory = sys_get_temp_dir() . '/doctrine_cache_'. uniqid();
        } while (file_exists($this->directory));
    }

    public function tearDown()
    {
        if ( ! is_dir($this->directory)) {
            return;
        }

        $iterator = new RecursiveDirectoryIterator($this->directory);

        foreach (new RecursiveIteratorIterator($iterator, RecursiveIteratorIterator::CHILD_FIRST) as $file) {
            if ($file->isFile()) {
                @unlink($file->getRealPath());
            } elseif ($file->isDir()) {
                @rmdir($file->getRealPath());
            }
        }
    }

    protected function isSharedStorage()
    {
        return false;
    }
}
