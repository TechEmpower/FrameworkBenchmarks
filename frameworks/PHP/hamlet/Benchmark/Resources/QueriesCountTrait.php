<?php

namespace Benchmark\Resources;

use Hamlet\Http\Requests\Request;
use function Hamlet\Cast\_int;

trait QueriesCountTrait
{
    protected function getQueriesCount(Request $request): int
    {
        if ($request->hasQueryParam('queries')) {
            $count = $request->getQueryParam('queries', _int());
            if ($count < 1) {
                return 1;
            } elseif (500 < $count) {
                return 500;
            } else {
                return $count;
            }
        } else {
            return 1;
        }
    }
}
