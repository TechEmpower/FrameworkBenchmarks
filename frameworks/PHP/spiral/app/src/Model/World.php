<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App\Model;

use App\Model\Repository;
use Cycle\Annotated\Annotation\Column;
use Cycle\Annotated\Annotation\Entity;

#[Entity(table: 'World', repository: Repository\WorldRepository::class)]
class World implements \JsonSerializable
{
    #[Column(type: 'primary')]
    public $id;

    #[Column(type: 'int', name: 'randomNumber')]
    public $randomNumber;

    /**
     * @return array
     */
    public function jsonSerialize(): mixed
    {
        return ['id' => $this->id, 'randomNumber' => $this->randomNumber];
    }
}
