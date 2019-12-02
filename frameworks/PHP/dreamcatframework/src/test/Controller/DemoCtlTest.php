<?php

namespace DreamCat\FrameDemo\Controller;

use DreamCat\FrameCore\ConfigReaderImpl;
use DreamCat\FrameCore\Factory\Impl\Container\DefaultContainerFactory;
use DreamCat\FrameDemo\Pojo\Demo\CountQuery\UserCountQueryInVo;
use PHPUnit\Framework\TestCase;

/**
 * demo控制器测试用例
 * @author vijay
 */
class DemoCtlTest extends TestCase
{
    /**
     * index方法的测试代码
     * @return void
     */
    public function testIndex()
    {
        $container = (new DefaultContainerFactory())->create(new ConfigReaderImpl(__DIR__ . "/../../../"));
        /** @var DemoCtl $ctl */
        $ctl = $container->get(DemoCtl::class);
        $outvo = $ctl->index((new UserCountQueryInVo())->setName("root"));
        # 此处应该结合业务逻辑做判断
        static::assertEquals($outvo, $outvo);
    }
}

# end of file
