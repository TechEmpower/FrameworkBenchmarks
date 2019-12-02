<?php

namespace DreamCat\FrameDemo\Entry\Mysql;

/**
 * mysql用户表的实体
 * @author vijay
 */
class UserEntry
{
    /**
     * @from Host
     * @var string $host 允许登录的主机
     */
    private $host;

    /**
     * @from User
     * @var string $name 登录名
     */
    private $name;

    /**
     * @return string 允许登录的主机
     */
    public function getHost(): string
    {
        return $this->host;
    }

    /**
     * @param string $host 允许登录的主机
     * @return static 对象本身
     */
    public function setHost(string $host): UserEntry
    {
        $this->host = $host;
        return $this;
    }

    /**
     * @return string 登录名
     */
    public function getName(): string
    {
        return $this->name;
    }

    /**
     * @param string $name 登录名
     * @return static 对象本身
     */
    public function setName(string $name): UserEntry
    {
        $this->name = $name;
        return $this;
    }
}

# end of file
