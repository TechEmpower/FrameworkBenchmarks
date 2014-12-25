<?php

namespace Skamander\BenchmarkBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity
 * @ORM\Table(name="World")
 */
class World /* implements JsonSerializable */
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
     * Although this is a benchmark, implemented this so
     * we don't have to make our members public. :)
     *
     * @return array
     */
    /*
    public function jsonSerialize() {
        return [
            'id' => $this->id,
            'randomNumber' => $this->randomNumber
        ];
    }
    */

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