<?php declare(strict_types=1);
/**
 * DuckPhp
 * From this time, you never be alone~
 */

namespace DuckPhpBenchmark\System;

use DuckPhp\ThrowOn\ThrowOn;
use DuckPhpBenchmark\System\App;

class BaseException
{
    use ThrowOn;
    
    public function display($ex)
    {
        App::OnDefaultException($ex);
    }
}
