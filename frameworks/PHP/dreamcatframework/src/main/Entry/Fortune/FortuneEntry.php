<?php

namespace DreamCat\Benchmark\Entry\Fortune;

/**
 * Fortune 的查询实体
 * @author vijay
 */
class FortuneEntry
{
    /** @var int */
    private $id;
    /** @var string */
    private $message;

    /**
     * @return int
     */
    public function getId(): int
    {
        return $this->id;
    }

    /**
     * @param int $id
     * @return FortuneEntry
     */
    public function setId(int $id): FortuneEntry
    {
        $this->id = $id;
        return $this;
    }

    /**
     * @return string
     */
    public function getMessage(): string
    {
        return $this->message;
    }

    /**
     * @param string $message
     * @return FortuneEntry
     */
    public function setMessage(string $message): FortuneEntry
    {
        $this->message = $message;
        return $this;
    }


}

# end of file
