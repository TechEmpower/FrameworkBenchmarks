<?php

namespace App\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity(repositoryClass="App\Repository\FortuneRepository")
 * @ORM\Table(name="fortune")
 */
class Fortune
{
    /**
     * @ORM\Id
     * @ORM\Column(type="integer")
     */
    public int $id = 0;

    /**
     * @ORM\Column(type="string")
     */
    public string $message = '';
}
