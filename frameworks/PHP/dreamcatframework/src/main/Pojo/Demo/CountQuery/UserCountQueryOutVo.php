<?php

namespace DreamCat\FrameDemo\Pojo\Demo\CountQuery;

/**
 * 查询用户数的输出vo
 * @author vijay
 */
class UserCountQueryOutVo
{
    /** @var int 条数 */
    private $count;

    /**
     * @return int 条数
     */
    public function getCount(): int
    {
        return $this->count;
    }

    /**
     * @param int $count 条数
     * @return static 对象本身
     */
    public function setCount(int $count): UserCountQueryOutVo
    {
        $this->count = $count;
        return $this;
    }
}

# end of file
