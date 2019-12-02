<?php

namespace DreamCat\FrameDemo\Pojo\Demo\UserQuery;

use DreamCat\FrameDemo\Entry\Mysql\UserEntry;

/**
 * 查询用户列表的输出VO
 * @author vijay
 */
class UserQueryOutVo
{
    /** @var UserEntry[] 查到的用户列表 */
    private $list;

    /**
     * @return UserEntry[] 查到的用户列表
     */
    public function getList(): array
    {
        return $this->list;
    }

    /**
     * @param UserEntry[] $list 查到的用户列表
     * @return UserQueryOutVo
     */
    public function setList(array $list): UserQueryOutVo
    {
        $this->list = $list;
        return $this;
    }
}

# end of file
