<?php

namespace DreamCat\Benchmark\Controller;

use DreamCat\Benchmark\Pojo\Json\JsonOutputVo;
use DreamCat\Benchmark\Service\JsonService;

/**
 * json_url
 * @author vijay
 */
class JsonCtl
{
    /**
     * @Autowire
     * @var JsonService
     */
    private $jsonService;

    /**
     * @return JsonService
     */
    public function getJsonService(): JsonService
    {
        return $this->jsonService;
    }

    /**
     * @param JsonService $jsonService
     * @return static self
     */
    public function setJsonService(JsonService $jsonService): JsonCtl
    {
        $this->jsonService = $jsonService;
        return $this;
    }

    /**
     * -
     * @return JsonOutputVo
     * @Get(/json)
     */
    public function index(): JsonOutputVo
    {
        return $this->jsonService->getJson();
    }
}

# end of file
