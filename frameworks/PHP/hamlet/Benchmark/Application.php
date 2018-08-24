<?php

namespace Benchmark;

use Benchmark\Entities\PlainTextEntity;
use Benchmark\Resources\DbResource;
use Benchmark\Resources\FortuneResource;
use Benchmark\Resources\UpdateResource;
use Cache\Adapter\Void\VoidCachePool;
use Hamlet\Applications\AbstractApplication;
use Hamlet\Database\Database;
use Hamlet\Entities\JsonEntity;
use Hamlet\Requests\Request;
use Hamlet\Resources\EntityResource;
use Hamlet\Resources\WebResource;
use Psr\Cache\CacheItemPoolInterface;

class Application extends AbstractApplication
{
    /** @var CacheItemPoolInterface|null */
    private $cache;

    /** @var Database|null */
    private $database;

    protected function findResource(Request $request): WebResource
    {
        if ($request->pathMatches('/plaintext')) {
            return new EntityResource(new PlainTextEntity('Hello, World!'), 'GET');
        } elseif ($request->pathMatches('/json')) {
            return new EntityResource(new JsonEntity([
                'message' => 'Hello, World!'
            ]), 'GET');
        } elseif ($request->pathMatches('/db') || $request->pathMatches('/queries')) {
            return new DbResource($this->database());
        } elseif ($request->pathMatches('/fortunes')) {
            return new FortuneResource($this->database());
        } elseif ($request->pathMatches('/update')) {
            return new UpdateResource($this->database());
        }
    }

    private function database(): Database
    {
        if (!$this->database) {
            $this->database = Database::mysql(
                'p:tfb-database',
                'benchmarkdbuser',
                'benchmarkdbpass',
                'hello_world'
            );
        }
        return $this->database;
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        if (!$this->cache) {
            $this->cache = new VoidCachePool();
        }
        return $this->cache;
    }
}
