<?php

declare(strict_types=1);

namespace App\Model;

use App\Model\Repository;
use Cycle\Annotated\Annotation\Column;
use Cycle\Annotated\Annotation\Entity;

#[Entity(table: 'fortune', repository: Repository\FortuneRepository::class)]
class Fortune implements \JsonSerializable
{
    #[Column(type: 'primary')]
    public int $id;

    #[Column(type: 'text')]
    public string $message;

    public function jsonSerialize(): array
    {
        return ['id' => $this->id, 'message' => $this->message];
    }
}
