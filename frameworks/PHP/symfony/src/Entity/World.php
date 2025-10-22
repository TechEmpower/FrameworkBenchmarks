<?php

namespace App\Entity;

use Doctrine\ORM\Mapping as ORM;
use Doctrine\DBAL\Types\Types;


#[ORM\Entity]
class World
{
    
    #[ORM\Id]
    #[ORM\Column(type: Types::INTEGER)]
    public int $id = 0;

    #[ORM\Column(type: Types::INTEGER)]

    public int $randomNumber = 0;
}
