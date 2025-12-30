<?php

declare(strict_types=1);

namespace App\Model;

use App\Model\Repository;
use Cycle\Annotated\Annotation\Column;
use Cycle\Annotated\Annotation\Entity;

#[Entity(table: 'world', repository: Repository\WorldRepository::class)]
class World implements \JsonSerializable
{
    #[Column(type: 'primary')]
    public int $id;

    #[Column(type: 'int', name: 'randomNumber')]
    public int $randomNumber;

    public function jsonSerialize(): array
    {
        return ['id' => $this->id, 'randomNumber' => $this->randomNumber];
    }
}
