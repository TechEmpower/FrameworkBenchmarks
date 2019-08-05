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

    public function setId(int $id): World
    {
        $this->id = $id;

        return $this;
    }

    public function getId(): int
    {
        return $this->id;
    }

    public function setRandomNumber(int $randomNumber): World
    {
        $this->randomNumber = $randomNumber;

        return $this;
    }

    public function getRandomNumber(): int
    {
        return $this->randomNumber;
    }
}