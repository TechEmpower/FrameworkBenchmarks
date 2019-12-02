<?php

namespace DreamCat\Benchmark\Model;

use DreamCat\Array2Class\Array2ClassInterface;
use Dreamcat\Components\Db\Mysql\SqlBuilder\SqlBuilder;
use DreamCat\FrameDbFactory\Model\AbstractModel;
use DreamCat\Benchmark\Entry\Mysql\UserEntry;

/**
 * 示例model
 * @author vijay
 */
class DemoMdl extends AbstractModel
{
    /** @var string 使用的表名 */
    private $tableName = "user";

    /**
     * -
     * @param string $nameKey 查询的信息关键字
     * @return int 条数
     */
    public function queryCount(string $nameKey): int
    {
        $sqlBuilder = SqlBuilder::count()
            ->table($this->tableName);
        if (strlen($nameKey)) {
            $sqlBuilder = $sqlBuilder->where(["User|has" => $nameKey]);
        }
        return $this->getMysql()->queryResult($sqlBuilder->build())->fetch()["c"];
    }


    /**
     * 查询某个或很多数据库用户信息
     * @param string $name 用户名，不传则不使用 where
     * @return UserEntry[] 用户数据，不超过500条
     */
    public function getUser(string $name = ""): array
    {
        $sqlBuilder = SqlBuilder::select()
            ->table($this->tableName)
            ->addCols(
                [
                    "Host",
                    "User",
                ]
            );
        if (strlen($name)) {
            $sqlBuilder->where(["User" => $name]);
        }
        $list = $this->getMysql()->queryResult($sqlBuilder->build())->fetchAll();
        return array_map(function ($v) {
            return $this->formatOutputEntry($v, UserEntry::class);
        }, $list);
    }
}

# end of file
