<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */

namespace DuckPhpBenchmark\Business;

use DuckPhpBenchmark\System\BaseBusiness;
use DuckPhpBenchmark\System\Helper\BusinessHelper as B;
use DuckPhpBenchmark\Model\WorldModel;
use DuckPhpBenchmark\Model\FortuneModel;

class DbBusiness extends BaseBusiness
{
    public function getRandomRow()
    {
        return WorldModel::G()->getRandomRow();
    }
    public function multiQuery($query_count)
    {
        return WorldModel::G()->multiQuery($query_count);
    }
    public function multiUpdate($query_count)
    {
        return WorldModel::G()->multiUpdate($query_count);
    }
    public function getFortunes()
    {
        return FortuneModel::G()->getFortunes();
    }
}
