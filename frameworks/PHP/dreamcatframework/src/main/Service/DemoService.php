<?php

namespace DreamCat\FrameDemo\Service;

use DreamCat\FrameDemo\Model\DemoMdl;
use DreamCat\FrameDemo\Pojo\Demo\CountQuery\UserCountQueryInVo;
use DreamCat\FrameDemo\Pojo\Demo\CountQuery\UserCountQueryOutVo;
use DreamCat\FrameDemo\Pojo\Demo\UserQuery\UserQueryInVo;
use DreamCat\FrameDemo\Pojo\Demo\UserQuery\UserQueryOutVo;

/**
 * 示例service
 * @author vijay
 */
class DemoService
{
    /**
     * @Autowire
     * @var DemoMdl $demoMdl 对应model
     */
    private $demoMdl;

    /**
     * @return DemoMdl 对应model
     */
    public function getDemoMdl(): DemoMdl
    {
        return $this->demoMdl;
    }

    /**
     * @param DemoMdl $demoMdl 对应model
     * @return static 对象本身
     */
    public function setDemoMdl(DemoMdl $demoMdl): DemoService
    {
        $this->demoMdl = $demoMdl;
        return $this;
    }

    /**
     * 查询用户信息
     * @param UserQueryInVo $inVo 查询条件
     * @return UserQueryOutVo 查询结果
     */
    public function queryUser(UserQueryInVo $inVo): UserQueryOutVo
    {
        return (new UserQueryOutVo())
            ->setList($this->demoMdl->getUser($inVo->getName()));
    }

    /**
     * 查询用户条数
     * @param UserCountQueryInVo $inVo 要查询的条件
     * @return UserCountQueryOutVo 查询结果
     */
    public function queryUserCount(UserCountQueryInVo $inVo): UserCountQueryOutVo
    {
        return (new UserCountQueryOutVo())->setCount($this->demoMdl->queryCount($inVo->getName()));
    }
}

# end of file
