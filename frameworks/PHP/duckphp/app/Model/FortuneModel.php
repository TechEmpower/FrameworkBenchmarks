<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */

namespace DuckPhpBenchmark\Model;

use DuckPhpBenchmark\System\BaseModel;
use DuckPhpBenchmark\System\Helper\ModelHelper as M;

class FortuneModel extends BaseModel
{
    public function getFortunes()
    {
        $sql = 'SELECT id, message FROM fortune';
        $data = M::DB()->fetchAll($sql);
        $ret = array_column($data, 'message', 'id');
        $ret[0] = 'Additional fortune added at request time.';
        asort($ret);
        return $ret;
    }
}
