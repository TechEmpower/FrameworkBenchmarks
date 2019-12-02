<?php

namespace DreamCat\FrameDemo\Pojo\Demo\UserQuery;

/**
 * 用户查询请求的VO
 * @author vijay
 */
class UserQueryInVo
{
    /** @var string 要查询的用户名 */
    private $name = "";

    /**
     * @return string 要查询的用户名
     */
    public function getName(): string
    {
        return $this->name;
    }

    /**
     * @param string $name 要查询的用户名
     * @return static 对象本身
     */
    public function setName(string $name): UserQueryInVo
    {
        $this->name = $name;
        return $this;
    }
}

# end of file
