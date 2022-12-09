<?php

declare(strict_types=1);

namespace ImiApp\Model\Base;

use Imi\Config\Annotation\ConfigValue;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;
use Imi\Model\Annotation\Table;
use Imi\Pgsql\Model\PgModel as Model;

/**
 * World 基类.
 *
 * @Entity
 * @Table(name=@ConfigValue(name="@app.models.ImiApp\Model\World.name", default="World"), usePrefix=false, id={"id"}, dbPoolName=@ConfigValue(name="@app.models.ImiApp\Model\World.poolName"))
 *
 * @property int|null $id 
 * @property int|null $randomnumber 
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

     * @Column(name="id", type="int4", length=-1, accuracy=0, nullable=false, default="", isPrimaryKey=true, primaryKeyIndex=1, isAutoIncrement=false, ndims=0, virtual=false)
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
     *
     * @param int|null $id id
     * @return static
     */
    public function setId(?int $id)
    {
        $this->id = $id;
        return $this;
    }

    /**
     * randomnumber.

     * @Column(name="randomnumber", type="int4", length=-1, accuracy=0, nullable=false, default="0", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false, ndims=0, virtual=false)
     * @var int|null
     */
    protected ?int $randomnumber = 0;

    /**
     * 获取 randomnumber.
     *
     * @return int|null
     */
    public function getRandomnumber(): ?int
    {
        return $this->randomnumber;
    }

    /**
     * 赋值 randomnumber.
     *
     * @param int|null $randomnumber randomnumber
     * @return static
     */
    public function setRandomnumber(?int $randomnumber)
    {
        $this->randomnumber = $randomnumber;
        return $this;
    }

}
