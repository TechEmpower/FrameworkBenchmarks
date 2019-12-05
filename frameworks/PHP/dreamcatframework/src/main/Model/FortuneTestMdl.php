<?php

namespace DreamCat\Benchmark\Model;

use DreamCat\Benchmark\Entry\Fortune\FortuneEntry;
use DreamCat\FrameDbFactory\Model\AbstractModel;

/**
 * Fortune表的model
 * @author vijay
 */
class FortuneTestMdl extends AbstractModel
{
    /**
     * 获取所有的Fortune数据
     * @return FortuneEntry[]
     */
    public function getAllMessage(): array
    {
        $list = [];
        foreach ($this->getMysql()->queryWithoutPrepare("select id, message from Fortune") as $row) {
            $list[] = $this->formatOutputEntry($row, FortuneEntry::class);
        }
        return $list;
    }
}

# end of file
