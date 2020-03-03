<?php

namespace App\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity(repositoryClass="App\Repository\WorldRepository")
 * @ORM\Table(name="world")
 */
class World
{
    /**
     * @ORM\Id
     * @ORM\Column(type="integer")
     */
    public int $id = 0;

    /**
     * @ORM\Column(type="integer")
     */
    public int $randomNumber = 0;
}
