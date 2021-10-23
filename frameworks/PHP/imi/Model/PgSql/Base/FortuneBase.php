<?php
declare(strict_types=1);

namespace ImiApp\Model\PgSql\Base;

use Imi\Pgsql\Model\PgModel as Model;
use Imi\Model\Annotation\Table;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;

/**
 * Fortune 基类
 * @Entity
 * @Table(name="Fortune", id={"id"}, dbPoolName="pgsql")
 * @property int|null $id 
 * @property string|null $message 
 */
abstract class FortuneBase extends Model
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
     * message
     * @Column(name="message", type="varchar", length=0, accuracy=2048, nullable=false, default="", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false, ndims=0)
     * @var string|null
     */
    protected ?string $message = null;

    /**
     * 获取 message
     *
     * @return string|null
     */
    public function getMessage(): ?string
    {
        return $this->message;
    }

    /**
     * 赋值 message
     * @param string|null $message message
     * @return static
     */
    public function setMessage(?string $message)
    {
        $this->message = $message;
        return $this;
    }

}
