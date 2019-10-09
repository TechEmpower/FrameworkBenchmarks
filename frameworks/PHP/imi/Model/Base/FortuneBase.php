<?php
namespace ImiApp\Model\Base;

use Imi\Model\Model;
use Imi\Model\Annotation\Table;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;

/**
 * FortuneBase
 * @Entity
 * @Table(name="fortune", id={"id"})
 * @property int $id 
 * @property string $message 
 */
abstract class FortuneBase extends Model
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
     * message
     * @Column(name="message", type="varchar", length=2048, accuracy=0, nullable=false, default="", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false)
     * @var string
     */
    protected $message;

    /**
     * 获取 message
     *
     * @return string
     */ 
    public function getMessage()
    {
        return $this->message;
    }

    /**
     * 赋值 message
     * @param string $message message
     * @return static
     */ 
    public function setMessage($message)
    {
        $this->message = $message;
        return $this;
    }

}
