<?php

namespace DreamCat\Benchmark\Model;

use DreamCat\Benchmark\Entry\World\WorldEntry;
use Dreamcat\Components\Db\Mysql\SqlBuilder\SqlBuilder;
use DreamCat\FrameDbFactory\Model\AbstractModel;

/**
 * world表测试用的model
 * @author vijay
 */
class WorldTestMdl extends AbstractModel
{
    const MAX_WORLD_ID = 10000;

    /**
     * 单次查询
     * @return WorldEntry 查询结果
     */
    public function singleQuery(): WorldEntry
    {
        $mysql = $this->getMysql();
        $result = $mysql->queryWithoutPrepare("select id, randomNumber from World where id = " . mt_rand(1, self::MAX_WORLD_ID));
        return $this->formatOutputEntry($result->fetch(), WorldEntry::class);
    }

    /**
     * 多次查询
     * @param int $queries 查询的条数
     * @return WorldEntry[] 查询结果
     */
    public function multipleQueries(int $queries): array
    {
        $mysql = $this->getMysql();
        $prepare = $mysql->prepare("select id, randomNumber from World where id = ?");
        $list = [];
        for ($idx = 0; $idx < $queries; ++$idx) {
            $prepare->bindValue([mt_rand(1, self::MAX_WORLD_ID)]);
            $list[] = $this->formatOutputEntry($prepare->selectrow(), WorldEntry::class);
        }
        return $list;
    }

    /**
     * 将不超过500条数据更新回数据库
     * @param WorldEntry[] $list
     * @return void
     */
    public function updates(array $list): void
    {
        $prepare = $this->getMysql()->prepare("update World set randomNumber = ? where id = ?");
        array_walk($list, function(WorldEntry $entry) use ($prepare) {
           $prepare->bindValue([$entry->getRandomNumber(), $entry->getId()]);
           $prepare->exec();
        });
        # $this->getMysql()->queryResult($sql); # 这里本来是准备用冲突更新的，但是测试不通过，可能是db未支持
    }
}

# end of file
