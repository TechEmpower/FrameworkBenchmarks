<?php

namespace Benchmark;

use Benchmark\Resources\{CachedQueriesResource, DbResource, FortuneResource, HelloJsonResource, HelloTextResource, QueriesResource, UpdateResource};
use Cache\Adapter\Apcu\ApcuCachePool;
use Hamlet\Database\Database;
use Hamlet\Http\Applications\AbstractApplication;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\{HttpResource, NotFoundResource};
use Psr\Cache\CacheItemPoolInterface;

class Application extends AbstractApplication
{
    public function __construct(private Database $database, private CacheItemPoolInterface|null $cache = null) {}

    public function findResource(Request $request): HttpResource
    {
        switch ($request->getPath()) {
            case '/plaintext':
                return new HelloTextResource;
            case '/json':
                return new HelloJsonResource;
            case '/db':
                return new DbResource($this->database);
            case '/queries':
                return new QueriesResource($this->database);
            case '/cached-worlds':
                return new CachedQueriesResource($this->getCache($request), $this->database);
            case '/fortunes':
                return new FortuneResource($this->database);
            case '/update':
                return new UpdateResource($this->database);
        }
        return new NotFoundResource;
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        if (!$this->cache) {
            $this->cache = new ApcuCachePool;
        }
        return $this->cache;
    }
}
