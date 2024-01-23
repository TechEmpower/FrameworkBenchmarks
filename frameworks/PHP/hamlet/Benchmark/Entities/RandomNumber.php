<?php

namespace Benchmark\Entities;

use Hamlet\Database\Entity;
use JsonSerializable;

final class RandomNumber implements Entity, JsonSerializable
{
    public function __construct(private int $id, private int $randomNumber) {}

    public function id(): int
    {
        return $this->id;
    }

    public function number(): int
    {
        return $this->randomNumber;
    }

    public function withNumber(int $number): self
    {
        return new self($this->id, $number);
    }

    public function jsonSerialize(): array
    {
        return [
            'id' => $this->id,
            'randomNumber' => $this->randomNumber
        ];
    }
}
