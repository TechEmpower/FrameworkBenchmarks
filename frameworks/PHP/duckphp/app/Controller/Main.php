<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */

namespace DuckPhpBenchmark\Controller;

use DuckPhpBenchmark\System\Helper\ControllerHelper as C;
use DuckPhpBenchmark\Business\DbBusiness;

class Main
{
    public function index()
    {
        echo 'hello! duckphp: '. DATE(DATE_ATOM);
    }
    // change if  you can
    public function plaintext()
    {
        C::header('Content-Type: text/plain; charset=utf-8');
        echo 'Hello, World!';
    }
    public function json()
    {
        $ret = [
            'message' => 'Hello, World!',
        ];
        C::ExitJson($ret);
    }
    public function db()
    {
        $ret = DbBusiness::G()->getRandomRow();
        C::DbCloseAll();
        C::ExitJson($ret);
    }
    public function updates()
    {
        $queries = (int) C::GET('queries',1);
        $query_count = 1;
        if ($queries > 1) {
            $query_count = min($queries, 500);
        }
        $ret = DbBusiness::G()->multiUpdate($query_count);
        C::DbCloseAll();
        C::ExitJson($ret);
    }
    public function queries()
    {
        $queries = (int) C::GET('queries',1);
        $query_count = 1;
        if ($queries > 1) {
            $query_count = min($queries, 500);
        }
        
        $ret = DbBusiness::G()->multiQuery($query_count);
        C::DbCloseAll();
        C::ExitJson($ret);
    }
    public function fortunes()
    {
        $arr = DbBusiness::G()->getFortunes();
        C::DbCloseAll();
        C::Show(get_defined_vars(),'fortunes');
    }
}
