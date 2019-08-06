<?php

namespace App\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity
 * @ORM\Table(name="world")
 */
class World
{
    /**
     * @ORM\Id
     * @ORM\Column(type="integer")
     */
    public $id;

    /**
     * @ORM\Column(type="integer")
     */
    public $randomNumber;

    public function setId(int $id): void
    {
        $this->id = $id;
    }

    public function getId(): int
    {
        return $this->id;
    }

    public function setRandomNumber(int $randomNumber): void
    {
        $this->randomNumber = $randomNumber;
    }

    public function getRandomNumber(): int
    {
        return $this->randomNumber;
    }
}