<?php

namespace DreamCat\Benchmark\Controller;

use DreamCat\Benchmark\Service\DbTestService;

class DbCtl
{
    /**
     * @Autowire
     * @var DbTestService
     */
    private $dbTestService;
    public function index()
    {
        return $this->dbTestService->singleQuery();
    }
}

# end of file
