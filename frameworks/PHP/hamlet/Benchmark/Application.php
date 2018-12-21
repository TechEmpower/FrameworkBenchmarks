<?php

namespace Benchmark;

use Benchmark\Resources\DbResource;
use Benchmark\Resources\FortuneResource;
use Benchmark\Resources\HelloResource;
use Benchmark\Resources\UpdateResource;
use Cache\Adapter\Apcu\ApcuCachePool;
use Hamlet\Applications\AbstractApplication;
use Hamlet\Database\Database;
use Hamlet\Requests\Request;
use Hamlet\Resources\WebResource;
use Psr\Cache\CacheItemPoolInterface;

class Application extends AbstractApplication
{
    /** @var CacheItemPoolInterface|null */
    private $cache;

    /** @var Database|null */
    private $database;

    public function findResource(Request $request): WebResource
    {
        if ($request->pathMatches('/plaintext')) {
            return new HelloResource(false);
        } elseif ($request->pathMatches('/json')) {
            return new HelloResource(true);
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
            $this->cache = new ApcuCachePool();
        }
        return $this->cache;
    }
}
