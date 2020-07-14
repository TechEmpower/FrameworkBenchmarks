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
 *     table="World",
 *     repository="Repository/WorldRepository"
 * )
 */
class World implements \JsonSerializable
{
    /** @Column(type="primary") */
    public $id;

    /** @Column(type="int", name="randomNumber") */
    public $randomNumber;

    /**
     * @return array|mixed
     */
    public function jsonSerialize()
    {
        return ['id' => $this->id, 'randomNumber' => $this->randomNumber];
    }
}