<?php

namespace Benchmark;

use Benchmark\Resources\DbResource;
use Benchmark\Resources\FortuneResource;
use Benchmark\Resources\HelloJsonResource;
use Benchmark\Resources\HelloTextResource;
use Benchmark\Resources\QueriesResource;
use Benchmark\Resources\UpdateResource;
use Cache\Adapter\PHPArray\ArrayCachePool;
use Hamlet\Database\Database;
use Hamlet\Http\Applications\AbstractApplication;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Resources\NotFoundResource;
use Psr\Cache\CacheItemPoolInterface;

class Application extends AbstractApplication
{
    /** @var CacheItemPoolInterface|null */
    private $cache;

    /** @var HttpResource */
    private $helloTextResource;

    /** @var HttpResource */
    private $helloJsonResource;

    /** @var HttpResource */
    private $dbResource;

    /** @var HttpResource */
    private $queriesResource;

    /** @var HttpResource */
    private $fortuneResource;

    /** @var HttpResource */
    private $updateResource;

    public function __construct(Database $database)
    {
        $this->helloJsonResource = new HelloJsonResource();
        $this->helloTextResource = new HelloTextResource();
        $this->dbResource        = new DbResource($database);
        $this->queriesResource   = new QueriesResource($database);
        $this->fortuneResource   = new FortuneResource($database);
        $this->updateResource    = new UpdateResource($database);
    }

    public function findResource(Request $request): HttpResource
    {
        switch ($request->getPath()) {
            case '/plaintext':
                return $this->helloTextResource;
            case '/json':
                return $this->helloJsonResource;
            case '/db':
                return $this->dbResource;
            case '/queries':
                return $this->queriesResource;
            case '/fortunes':
                return $this->fortuneResource;
            case '/update':
                return $this->updateResource;
        }
        return new NotFoundResource();
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        if (!$this->cache) {
            $this->cache = new ArrayCachePool();
        }
        return $this->cache;
    }
}
