<?php

declare(strict_types=1);

namespace ImiApp\Model\PgSql\Base;

use Imi\Config\Annotation\ConfigValue;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;
use Imi\Model\Annotation\Table;
use Imi\Pgsql\Model\PgModel as Model;

/**
 * fortune 基类.
 *
 * @Entity
 * @Table(name=@ConfigValue(name="@app.models.ImiApp\Model\PgSql\Fortune.name", default="fortune"), usePrefix=false, id={"id"}, dbPoolName=@ConfigValue(name="@app.models.ImiApp\Model\PgSql\Fortune.poolName"))
 *
 * @property int|null $id 
 * @property string|null $message 
 */
abstract class FortuneBase extends Model
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
     * message.

     * @Column(name="message", type="varchar", length=0, accuracy=2048, nullable=false, default="", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false, ndims=0, virtual=false)
     * @var string|null
     */
    protected ?string $message = NULL;

    /**
     * 获取 message.
     *
     * @return string|null
     */
    public function getMessage(): ?string
    {
        return $this->message;
    }

    /**
     * 赋值 message.
     *
     * @param string|null $message message
     * @return static
     */
    public function setMessage(?string $message)
    {
        $this->message = $message;
        return $this;
    }

}
