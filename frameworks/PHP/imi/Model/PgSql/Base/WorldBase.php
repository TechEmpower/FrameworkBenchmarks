<?php
declare(strict_types=1);

namespace ImiApp\Model\PgSql\Base;

use Imi\Pgsql\Model\PgModel as Model;
use Imi\Model\Annotation\Table;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;

/**
 * World 基类
 * @Entity(bean=false)
 * @Table(name="World", id={"id"}, dbPoolName="pgsql")
 * @property int|null $id 
 * @property int|null $randomnumber 
 */
abstract class WorldBase extends Model
{
    /**
     * id
     * @Column(name="id", type="int4", length=-1, accuracy=0, nullable=false, default="", isPrimaryKey=true, primaryKeyIndex=1, isAutoIncrement=false, ndims=0)
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
    public function setId(?int $id)
    {
        $this->id = $id;
        return $this;
    }

    /**
     * randomnumber
     * @Column(name="randomnumber", type="int4", length=-1, accuracy=0, nullable=false, default="0", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false, ndims=0)
     * @var int|null
     */
    protected ?int $randomnumber = null;

    /**
     * 获取 randomnumber
     *
     * @return int|null
     */
    public function getRandomnumber(): ?int
    {
        return $this->randomnumber;
    }

    /**
     * 赋值 randomnumber
     * @param int|null $randomnumber randomnumber
     * @return static
     */
    public function setRandomnumber(?int $randomnumber)
    {
        $this->randomnumber = $randomnumber;
        return $this;
    }

}
