<?php

namespace DreamCat\Benchmark\Service;

use DreamCat\Benchmark\Entry\World\WorldEntry;
use DreamCat\Benchmark\Model\WorldTestMdl;

/**
 * world测试service
 * @author vijay
 */
class WorldTestService
{
    /**
     * @Autowire
     * @var WorldTestMdl
     */
    private $worldTestMdl;

    /**
     * @return WorldTestMdl
     */
    public function getWorldTestMdl(): WorldTestMdl
    {
        return $this->worldTestMdl;
    }

    /**
     * @param WorldTestMdl $worldTestMdl
     * @return WorldTestService
     */
    public function setWorldTestMdl(WorldTestMdl $worldTestMdl): WorldTestService
    {
        $this->worldTestMdl = $worldTestMdl;
        return $this;
    }

    /**
     * 单次查询测试
     * @return WorldEntry
     */
    public function singleQuery(): WorldEntry
    {
        return $this->worldTestMdl->singleQuery();
    }

    /**
     * 多次查询
     * @param int $queries 查询的条数
     * @return WorldEntry[] 查询结果
     */
    public function multipleQueries(int $queries): array
    {
        return $this->worldTestMdl->multipleQueries($queries);
    }

    /**
     * 更新数据
     * @param int $queries 条数
     * @return WorldEntry[] 更新后结果
     */
    public function updates(int $queries): array
    {
        $list = $this->worldTestMdl->multipleQueries($queries);
        foreach ($list as $idx => $entry) {
            $list[$idx] = $entry->setRandomNumber(mt_rand(1, 10000));
        }
        $this->worldTestMdl->updates($list);
        return $list;
    }
}

# end of file
