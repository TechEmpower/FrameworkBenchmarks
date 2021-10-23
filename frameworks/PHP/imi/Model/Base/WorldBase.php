<?php
declare(strict_types=1);

namespace ImiApp\Model\Base;

use Imi\Model\Model as Model;
use Imi\Model\Annotation\DDL;
use Imi\Model\Annotation\Table;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;

/**
 * world 基类
 * @Entity
 * @Table(name="world", id={"id"})
 * @DDL(sql="CREATE TABLE `world` (   `id` int(10) unsigned NOT NULL AUTO_INCREMENT,   `randomNumber` int(11) NOT NULL DEFAULT '0',   PRIMARY KEY (`id`) ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci", decode="")
 * @property int|null $id 
 * @property int|null $randomNumber 
 */
abstract class WorldBase extends Model
{
    /**
     * id
     * @Column(name="id", type="int", length=10, accuracy=0, nullable=false, default="", isPrimaryKey=true, primaryKeyIndex=0, isAutoIncrement=true)
     * @var int|null
     */
    protected ?int $id = null;

    /**
     * 获取 id
     *
     * @return int|null
     */
    public function getId(): ?int
    {
        return $this->id;
    }

    /**
     * 赋值 id
     * @param int|null $id id
     * @return static
     */
    public function setId($id)
    {
        $this->id = null === $id ? null : (int)$id;
        return $this;
    }

    /**
     * randomNumber
     * @Column(name="randomNumber", type="int", length=11, accuracy=0, nullable=false, default="0", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false)
     * @var int|null
     */
    protected ?int $randomNumber = null;

    /**
     * 获取 randomNumber
     *
     * @return int|null
     */
    public function getRandomNumber(): ?int
    {
        return $this->randomNumber;
    }

    /**
     * 赋值 randomNumber
     * @param int|null $randomNumber randomNumber
     * @return static
     */
    public function setRandomNumber($randomNumber)
    {
        $this->randomNumber = null === $randomNumber ? null : (int)$randomNumber;
        return $this;
    }

}
