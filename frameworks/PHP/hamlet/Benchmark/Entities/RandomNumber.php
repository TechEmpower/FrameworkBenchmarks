<?php

namespace Benchmark\Entities;

use Hamlet\Database\Entity;
use JsonSerializable;

class RandomNumber implements Entity, JsonSerializable
{
    /** @var int */
    private $id;

    /** @var int */
    private $randomNumber;

    public function __construct(int $id, int $randomNumber)
    {
        $this->id = $id;
        $this->randomNumber = $randomNumber;
    }

    public function id(): int
    {
        return $this->id;
    }

    public function number(): int
    {
        return $this->randomNumber;
    }

    public function withNumber(int $number): RandomNumber
    {
        return new self($this->id, $number);
    }

    public function jsonSerialize()
    {
        return [
            'id' => $this->id,
            'randomNumber' => $this->randomNumber
        ];
    }
}
