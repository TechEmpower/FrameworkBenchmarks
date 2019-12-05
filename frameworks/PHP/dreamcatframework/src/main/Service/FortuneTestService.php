<?php

namespace DreamCat\Benchmark\Service;

use DreamCat\Benchmark\Entry\Fortune\FortuneEntry;
use DreamCat\Benchmark\Model\FortuneTestMdl;

/**
 * fortune测试服务
 * @author vijay
 */
class FortuneTestService
{
    /**
     * @Autowire
     * @var FortuneTestMdl
     */
    private $fortuneTestMdl;

    /**
     * @return FortuneTestMdl
     */
    public function getFortuneTestMdl(): FortuneTestMdl
    {
        return $this->fortuneTestMdl;
    }

    /**
     * @param FortuneTestMdl $fortuneTestMdl
     * @return FortuneTestService
     */
    public function setFortuneTestMdl(FortuneTestMdl $fortuneTestMdl): FortuneTestService
    {
        $this->fortuneTestMdl = $fortuneTestMdl;
        return $this;
    }

    /**
     * 获取所有的信息
     * @return FortuneEntry[]
     */
    public function getAllMessages(): array
    {
        $list = $this->getFortuneTestMdl()->getAllMessage();
        $list[] = (new FortuneEntry())->setId(0)->setMessage("Additional fortune added at request time.");

        usort($list, function (FortuneEntry $v1, FortuneEntry $v2) {
            return $v1->getMessage() <=> $v2->getMessage();
        });

        return $list;
    }
}

# end of file
