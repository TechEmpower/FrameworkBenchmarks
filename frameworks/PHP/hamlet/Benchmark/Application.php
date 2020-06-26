<?php

namespace Benchmark;

use Benchmark\Resources\{DbResource, FortuneResource, HelloJsonResource, HelloTextResource, QueriesResource, UpdateResource};
use Cache\Adapter\PHPArray\ArrayCachePool;
use Hamlet\Database\Database;
use Hamlet\Http\Applications\AbstractApplication;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\{HttpResource, NotFoundResource};
use Hamlet\Http\Responses\{Response, ServerErrorResponse};
use Psr\Cache\CacheItemPoolInterface;

class Application extends AbstractApplication
{
    /** @var CacheItemPoolInterface */
    private $cache;

    /** @var Database */
    private $database;

    public function __construct(Database $database)
    {
        $this->cache = new ArrayCachePool;
        $this->database = $database;
    }

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
            case '/fortunes':
                return new FortuneResource($this->database);
            case '/update':
                return new UpdateResource($this->database);
        }
        return new NotFoundResource;
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        return $this->cache;
    }
}
