<?php

namespace Benchmark\Resources;

use Cache\Adapter\Common\CacheItem;
use Hamlet\Database\Database;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\Response;
use Psr\Cache\CacheItemPoolInterface;

class CachedQueriesResource extends QueriesResource
{
    public function __construct(private CacheItemPoolInterface $cache, Database $database)
    {
        parent::__construct($database);
    }

    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);
        $key = 'count.' . $count;
        $item = $this->cache->getItem($key);
        if ($item->isHit()) {
            return $item->get();
        }
        $response = parent::getResponse($request);
        $this->cache->save(new CacheItem($key, true, $response));
        return $response;
    }
}
