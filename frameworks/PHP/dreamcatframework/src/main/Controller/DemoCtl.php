<?php

namespace DreamCat\FrameDemo\Controller;

use DreamCat\FrameDemo\Pojo\Demo\CountQuery\UserCountQueryInVo;
use DreamCat\FrameDemo\Pojo\Demo\CountQuery\UserCountQueryOutVo;
use DreamCat\FrameDemo\Pojo\Demo\UserQuery\UserQueryInVo;
use DreamCat\FrameDemo\Pojo\Demo\UserQuery\UserQueryOutVo;
use DreamCat\FrameDemo\Service\DemoService;

/**
 * 示例控制器
 * @author vijay
 * @Uri(/demo)
 */
class DemoCtl
{
    /**
     * @Autowire
     * @var DemoService
     */
    private $demoService;

    /**
     * @return DemoService
     */
    public function getDemoService(): DemoService
    {
        return $this->demoService;
    }

    /**
     * @param DemoService $demoService
     * @return DemoCtl
     */
    public function setDemoService(DemoService $demoService): DemoCtl
    {
        $this->demoService = $demoService;
        return $this;
    }

    /**
     * 不指定方法时的默认入口，查询有多少用户数
     * @param UserCountQueryInVo $vo @GetParam 查询数据
     * @return UserCountQueryOutVo
     * @Get(/count)
     */
    public function index(UserCountQueryInVo $vo): UserCountQueryOutVo
    {
        return $this->demoService->queryUserCount($vo);
    }

    /**
     * 指定了方法的入口
     * @param UserQueryInVo $inVo @PathVariable
     * @return UserQueryOutVo
     * @Get(/query/{name})
     */
    public function queryUser(UserQueryInVo $inVo): UserQueryOutVo
    {
        return $this->demoService->queryUser($inVo);
    }
}

# end of file
