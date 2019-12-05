<?php

namespace DreamCat\Benchmark\Service;

use DreamCat\Benchmark\Entry\World\SingleQueryEntry;
use DreamCat\Benchmark\Model\DbTestMdl;

/**
 * db测试service
 * @author vijay
 */
class DbTestService
{
    /**
     * @Autowire
     * @var DbTestMdl
     */
    private $mdl;

    /**
     * 单次查询测试
     * @return SingleQueryEntry
     */
    public function singleQuery(): SingleQueryEntry
    {
        return $this->mdl->singleQuery();
    }
    /**
     * 多次查询
     * @param int $queries 查询的条数
     * @return SingleQueryEntry[] 查询结果
     */
    public function multipleQueries(int $queries):array
    {
        return $this->mdl->multipleQueries($queries);
    }
}

# end of file
