<?php

namespace DreamCat\Benchmark\Model;

use DreamCat\Benchmark\Entry\World\SingleQueryEntry;
use DreamCat\FrameDbFactory\Model\AbstractModel;

/**
 * db测试用的model
 * @author vijay
 */
class DbTestMdl extends AbstractModel
{
    const MAX_WORLD_ID = 10000;
    /**
     * 单次查询
     * @return SingleQueryEntry 查询结果
     */
    public function singleQuery(): SingleQueryEntry
    {
        $mysql = $this->getMysql();
        $result = $mysql->queryWithoutPrepare("select id, randomNumber from World where id = " . mt_rand(1, self::MAX_WORLD_ID));
        return $this->formatOutputEntry($result->fetch(), SingleQueryEntry::class);
    }

    /**
     * 多次查询
     * @param int $queries 查询的条数
     * @return SingleQueryEntry[] 查询结果
     */
    public function multipleQueries(int $queries):array
    {
        $mysql = $this->getMysql();
        $prepare = $mysql->prepare("select id, randomNumber from World where id = ?");
        $list = [];
        for ($idx = 0; $idx < $queries ; ++$idx) {
            $prepare->bindValue([mt_rand(1, self::MAX_WORLD_ID)]);
            $list[] = $this->formatOutputEntry($prepare->selectrow(), SingleQueryEntry::class);
        }
        return $list;
    }
}

# end of file
