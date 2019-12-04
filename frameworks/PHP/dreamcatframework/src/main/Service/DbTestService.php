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
}

# end of file
