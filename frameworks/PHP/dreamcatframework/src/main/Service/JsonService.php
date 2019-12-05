<?php

namespace DreamCat\Benchmark\Service;

use DreamCat\Benchmark\Pojo\Json\JsonOutputVo;

/**
 * json service
 * @author vijay
 */
class JsonService
{
    /**
     * 获取json
     * @return JsonOutputVo
     */
    public function getJson(): JsonOutputVo
    {
        return (new JsonOutputVo())->setMessage("Hello, World!");
    }
}

# end of file
