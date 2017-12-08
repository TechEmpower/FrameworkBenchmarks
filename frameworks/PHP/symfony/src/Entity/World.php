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

    /**
     * Set id
     *
     * @param integer $id
     * @return World
     */
    public function setId($id)
    {
        $this->id = $id;
    
        return $this;
    }

    /**
     * Get id
     *
     * @return integer 
     */
    public function getId()
    {
        return $this->id;
    }

    /**
     * Set randomNumber
     *
     * @param integer $randomNumber
     * @return World
     */
    public function setRandomNumber($randomNumber)
    {
        $this->randomNumber = $randomNumber;
    
        return $this;
    }

    /**
     * Get randomNumber
     *
     * @return integer 
     */
    public function getRandomNumber()
    {
        return $this->randomNumber;
    }
}