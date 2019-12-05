<?php

namespace DreamCat\Benchmark\Controller;

use DreamCat\Benchmark\Entry\World\SingleQueryEntry;
use DreamCat\Benchmark\Service\DbTestService;

/**
 * Db测试用的控制器
 * @author vijay
 */
class DbTestCtl
{
    /**
     * @Autowire
     * @var DbTestService
     */
    private $dbTestService;

    /**
     * 单次查询
     * @return SingleQueryEntry
     */
    public function index(): SingleQueryEntry
    {
        return $this->dbTestService->singleQuery();
    }

    /**
     * -
     * @param int $queries @GetParam(queries)
     * @return SingleQueryEntry[]
     */
    public function queries(int $queries): array
    {
        if ($queries < 1) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }
        return $this->dbTestService->multipleQueries($queries);
    }
}

# end of file
