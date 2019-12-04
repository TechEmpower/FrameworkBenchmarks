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
    /**
     * 单次查询
     * @return SingleQueryEntry 查询结果
     */
    public function singleQuery(): SingleQueryEntry
    {
        $mysql = $this->getMysql();
        $result = $mysql->queryWithoutPrepare("select id, randomNumber from World where id = " . mt_rand(1, 10000));
        return $this->formatOutputEntry($result->fetch(), SingleQueryEntry::class);
    }
}

# end of file
