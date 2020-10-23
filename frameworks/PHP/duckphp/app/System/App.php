<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */

namespace DuckPhpBenchmark\System;

use DuckPhp\DuckPhp;

class App extends DuckPhp
{
    //@override
    public $options = [
        'use_setting_file' => true,

        'path_config' => 'app/config',
        'path_view' => 'app/view',
        //'skip_setting_file' => false,
        //'is_debug' => false, // debug mode is turn off
        //'error_404' => '_sys/error_404',     //  we use system error view
        //'error_500' => '_sys/error_500',     //  we use system error view
        //'error_debug' => '_sys/error_debug', //  we use system error view
    ];
    //@override
    public function _ExitJson($ret, $exit = true)
    {
        // oh no , duckphp v1.2.7 is : static::header('Content-Type:text/json'); we hotfix it.
        
        static::header('Content-Type:application/json');

        $flag = JSON_UNESCAPED_UNICODE | JSON_NUMERIC_CHECK;
        if ($this->_IsDebug()) {
            $flag = $flag | JSON_PRETTY_PRINT;
        }
        echo json_encode($ret, $flag);
        if ($exit) {
            static::exit();
        }
    }
}
