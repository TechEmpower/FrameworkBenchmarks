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

#[Entity(table: 'Fortune', repository: Repository\FortuneRepository::class)]
class Fortune implements \JsonSerializable
{
    #[Column(type: 'primary')]
    public $id;

    #[Column(type: 'text')]
    public $message;

    /**
     * @return array
     */
    public function jsonSerialize(): mixed
    {
        return ['id' => $this->id, 'message' => $this->message];
    }
}
