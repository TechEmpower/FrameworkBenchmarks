<?php

namespace DreamCat\Benchmark\Controller;

use DreamCat\Benchmark\Entry\World\WorldEntry;
use DreamCat\Benchmark\Service\WorldTestService;

/**
 * Db测试用的控制器
 * @author vijay
 */
class WorldTestCtl
{
    /**
     * @Autowire
     * @var WorldTestService
     */
    private $worldTestService;

    /**
     * @return WorldTestService
     */
    public function getWorldTestService(): WorldTestService
    {
        return $this->worldTestService;
    }

    /**
     * @param WorldTestService $worldTestService
     * @return WorldTestCtl
     */
    public function setWorldTestService(WorldTestService $worldTestService): WorldTestCtl
    {
        $this->worldTestService = $worldTestService;
        return $this;
    }

    /**
     * 单次查询
     * @return WorldEntry
     */
    public function index(): WorldEntry
    {
        return $this->worldTestService->singleQuery();
    }

    /**
     * -
     * @param int $queries @GetParam(queries)
     * @return WorldEntry[]
     */
    public function queries(int $queries = 1): array
    {
        if ($queries < 1) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }
        return $this->worldTestService->multipleQueries($queries);
    }

    /**
     * -
     * @param int $queries
     * @return WorldEntry[]
     */
    public function updates(int $queries = 1): array
    {
        if ($queries < 1) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }
        return $this->getWorldTestService()->updates($queries);
    }
}

# end of file
