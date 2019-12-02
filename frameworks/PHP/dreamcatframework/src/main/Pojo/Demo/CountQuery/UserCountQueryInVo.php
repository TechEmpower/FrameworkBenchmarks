<?php

namespace DreamCat\FrameDemo\Pojo\Demo\CountQuery;

/**
 * 查询用户条数的输入vo
 * @author vijay
 */
class UserCountQueryInVo
{
    /** @var string 要查询的名称部分关键字 */
    private $name = "";

    /**
     * @return string 要查询的名称部分关键字
     */
    public function getName(): string
    {
        return $this->name;
    }

    /**
     * @param string $name 要查询的名称部分关键字
     * @return static 对象本身
     */
    public function setName(string $name): UserCountQueryInVo
    {
        $this->name = $name;
        return $this;
    }
}

# end of file
