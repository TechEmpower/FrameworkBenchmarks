<?php

namespace Benchmark\Entities;

use Hamlet\Database\Entity;
use JsonSerializable;

class Message implements Entity, JsonSerializable
{
    /** @var int */
    private $id;

    /** @var string */
    private $message;

    public function __construct(int $id, string $message)
    {
        $this->id = $id;
        $this->message = $message;
    }

    public function id(): int
    {
        return $this->id;
    }

    public function message(): string
    {
        return $this->message;
    }

    public function jsonSerialize()
    {
        return [
            'id' => $this->id,
            'message' => $this->message
        ];
    }
}
