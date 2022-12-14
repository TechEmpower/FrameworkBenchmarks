<?php

declare(strict_types=1);

namespace ImiApp\Model\Base;

use Imi\Config\Annotation\ConfigValue;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\DDL;
use Imi\Model\Annotation\Entity;
use Imi\Model\Annotation\Table;
use Imi\Model\Model as Model;

/**
 * world 基类.
 *
 * @Entity(camel=true, bean=false, incrUpdate=false)
 * @Table(name=@ConfigValue(name="@app.models.ImiApp\Model\World.name", default="world"), usePrefix=false, id={"id"}, dbPoolName=@ConfigValue(name="@app.models.ImiApp\Model\World.poolName"))
 * @DDL(sql="CREATE TABLE `world` (   `id` int(10) unsigned NOT NULL AUTO_INCREMENT,   `randomNumber` int(11) NOT NULL DEFAULT '0',   PRIMARY KEY (`id`) ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci", decode="")
 *
 * @property int|null $id 
 * @property int|null $randomNumber 
 */
abstract class WorldBase extends Model
{
    /**
     * {@inheritdoc}
     */
    public const PRIMARY_KEY = 'id';

    /**
     * {@inheritdoc}
     */
    public const PRIMARY_KEYS = ["id"];

    /**
     * id.
     * @Column(name="id", type="int", length=10, accuracy=0, nullable=false, default="", isPrimaryKey=true, primaryKeyIndex=0, isAutoIncrement=true, unsigned=true, virtual=false)
     * @var int|null
     */
    protected ?int $id = NULL;

    /**
     * 获取 id.
     *
     * @return int|null
     */
    public function getId(): ?int
    {
        return $this->id;
    }

    /**
     * 赋值 id.
     * @param int|null $id id
     * @return static
     */
    public function setId($id)
    {
        $this->id = null === $id ? null : (int)$id;
        return $this;
    }

    /**
     * randomNumber.
     * @Column(name="randomNumber", type="int", length=11, accuracy=0, nullable=false, default="0", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false, unsigned=false, virtual=false)
     * @var int|null
     */
    protected ?int $randomNumber = 0;

    /**
     * 获取 randomNumber.
     *
     * @return int|null
     */
    public function getRandomNumber(): ?int
    {
        return $this->randomNumber;
    }

    /**
     * 赋值 randomNumber.
     * @param int|null $randomNumber randomNumber
     * @return static
     */
    public function setRandomNumber($randomNumber)
    {
        $this->randomNumber = null === $randomNumber ? null : (int)$randomNumber;
        return $this;
    }

}
