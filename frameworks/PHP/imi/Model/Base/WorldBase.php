<?php
namespace ImiApp\Model\Base;

use Imi\Model\Model;
use Imi\Model\Annotation\Table;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;

/**
 * WorldBase
 * @Entity
 * @Table(name="world", id={"id"})
 * @property int $id 
 * @property int $randomNumber 
 */
abstract class WorldBase extends Model
{
    /**
     * id
     * @Column(name="id", type="int", length=10, accuracy=0, nullable=false, default="", isPrimaryKey=true, primaryKeyIndex=0, isAutoIncrement=true)
     * @var int
     */
    protected $id;

    /**
     * 获取 id
     *
     * @return int
     */ 
    public function getId()
    {
        return $this->id;
    }

    /**
     * 赋值 id
     * @param int $id id
     * @return static
     */ 
    public function setId($id)
    {
        $this->id = $id;
        return $this;
    }

    /**
     * randomNumber
     * @Column(name="randomNumber", type="int", length=11, accuracy=0, nullable=false, default="0", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false)
     * @var int
     */
    protected $randomNumber;

    /**
     * 获取 randomNumber
     *
     * @return int
     */ 
    public function getRandomNumber()
    {
        return $this->randomNumber;
    }

    /**
     * 赋值 randomNumber
     * @param int $randomNumber randomNumber
     * @return static
     */ 
    public function setRandomNumber($randomNumber)
    {
        $this->randomNumber = $randomNumber;
        return $this;
    }

}
