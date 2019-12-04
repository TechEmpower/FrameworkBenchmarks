<?php

namespace DreamCat\Benchmark\Entry\World;

/**
 * 单次查询用的数据库实体
 * @author vijay
 */
class SingleQueryEntry
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
     * @return SingleQueryEntry
     */
    public function setId(int $id): SingleQueryEntry
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
     * @return SingleQueryEntry
     */
    public function setRandomNumber(int $randomNumber): SingleQueryEntry
    {
        $this->randomNumber = $randomNumber;
        return $this;
    }
}

# end of file
