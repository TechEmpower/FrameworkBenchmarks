<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */

namespace DuckPhpBenchmark\Model;

use DuckPhpBenchmark\System\BaseModel;
use DuckPhpBenchmark\System\Helper\ModelHelper as M;

class WorldModel extends BaseModel
{
    public function getRandomRow()
    {
        $sql = 'SELECT id,randomNumber FROM world WHERE id = '. mt_rand(1, 10000);
        $ret = M::DB()->fetch($sql);
        
        return $ret;
    }
    public function multiQuery($query_count)
    {
        $arr=[];
        while ($query_count--) {
            $sql = 'SELECT id,randomNumber FROM world WHERE id = ?';
            $data = M::DB()->fetch($sql,mt_rand(1, 10000));
            $arr[] = $data;
        }
        
        return $arr;
    }
    public function multiUpdate($query_count)
    {
        // port from php
        // For each query, store the result set values in the response array
        $arr=[];
        while ($query_count--) {
            $id = mt_rand(1, 10000);
            
            
            $sql = 'SELECT randomNumber, id FROM world WHERE id=?';
            $row = M::DB()->fetch($sql,$id);
            if (empty($row)) {
                continue;
            }
            $randomNumber = $row['randomNumber'];

            // Store result in array.
            $world = ['id' => $id, 'randomNumber' => $randomNumber];
            $world['randomNumber'] = mt_rand(1, 10000);
            
            $sql = 'UPDATE world SET randomNumber=? WHERE id=?';
            M::DB()->execute($sql, $world['randomNumber'],  $id);
            $arr[] = $world;
        }
        return $arr;
    
    }
}
