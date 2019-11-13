<?php

namespace Benchmark;

use Benchmark\Resources\{DbResource, FortuneResource, HelloJsonResource, HelloTextResource, QueriesResource, UpdateResource};
use Cache\Adapter\PHPArray\ArrayCachePool;
use Hamlet\Database\{Database, Session};
use Hamlet\Http\Applications\AbstractApplication;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\{HttpResource, NotFoundResource};
use Hamlet\Http\Responses\{Response, ServerErrorResponse};
use Psr\Cache\CacheItemPoolInterface;
use Throwable;

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

    public function run(Request $request): Response
    {
        try {
            return parent::run($request);
        } catch (Throwable $e) {
            return new ServerErrorResponse;
        }
    }

    public function findResource(Request $request): HttpResource
    {
        switch ($request->getPath()) {
            case '/plaintext':
                return new HelloTextResource;
            case '/json':
                return new HelloJsonResource;
            case '/db':
                return $this->database->withSession(function (Session $session) {
                    return new DbResource($session);
                });
            case '/queries':
                return $this->database->withSession(function (Session $session) {
                    return new QueriesResource($session);
                });
            case '/fortunes':
                return $this->database->withSession(function (Session $session) {
                    return new FortuneResource($session);
                });
            case '/update':
                return $this->database->withSession(function (Session $session) {
                    return new UpdateResource($session);
                });
        }
        return new NotFoundResource;
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        return $this->cache;
    }
}
