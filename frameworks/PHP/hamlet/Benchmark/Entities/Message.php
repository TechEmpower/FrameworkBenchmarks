<?php

namespace Benchmark\Entities;

use Hamlet\Database\Entity;
use JsonSerializable;

class Message implements Entity, JsonSerializable
{
    public function __construct(private int $id, private string $message) {}

    public function id(): int
    {
        return $this->id;
    }

    public function message(): string
    {
        return $this->message;
    }

    public function jsonSerialize(): array
    {
        return [
            'id' => $this->id,
            'message' => $this->message
        ];
    }
}
