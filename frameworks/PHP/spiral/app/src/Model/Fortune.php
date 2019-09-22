<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App\Model;

use Cycle\Annotated\Annotation\Column;
use Cycle\Annotated\Annotation\Entity;

/**
 * @Entity(
 *     table="Fortune",
 *     repository="Repository/FortuneRepository"
 * )
 */
class Fortune implements \JsonSerializable
{
    /** @Column(type="primary") */
    public $id;

    /** @Column(type="text") */
    public $message;

    /**
     * @return array|mixed
     */
    public function jsonSerialize()
    {
        return ['id' => $this->id, 'message' => $this->message];
    }
}