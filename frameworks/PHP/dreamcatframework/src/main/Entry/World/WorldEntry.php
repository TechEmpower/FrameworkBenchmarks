<?php

namespace DreamCat\Benchmark\Entry\World;

/**
 * World 查询用的数据库实体
 * @author vijay
 */
class WorldEntry
{
    /** @var int */
    private $id;
    /** @var int */
    private $randomNumber;

    /**
     * @return int
     */
    public function getId(): int
    {
        return $this->id;
    }

    /**
     * @param int $id
     * @return WorldEntry
     */
    public function setId(int $id): WorldEntry
    {
        $this->id = $id;
        return $this;
    }

    /**
     * @return int
     */
    public function getRandomNumber(): int
    {
        return $this->randomNumber;
    }

    /**
     * @param int $randomNumber
     * @return WorldEntry
     */
    public function setRandomNumber(int $randomNumber): WorldEntry
    {
        $this->randomNumber = $randomNumber;
        return $this;
    }
}

# end of file
