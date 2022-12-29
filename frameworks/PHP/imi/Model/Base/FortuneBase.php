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
 * fortune 基类.
 *
 * @Entity(camel=true, bean=false, incrUpdate=false)
 * @Table(name=@ConfigValue(name="@app.models.ImiApp\Model\Fortune.name", default="fortune"), usePrefix=false, id={"id"}, dbPoolName=@ConfigValue(name="@app.models.ImiApp\Model\Fortune.poolName"))
 * @DDL(sql="CREATE TABLE `fortune` (   `id` int(10) unsigned NOT NULL AUTO_INCREMENT,   `message` varchar(2048) CHARACTER SET utf8 NOT NULL,   PRIMARY KEY (`id`) ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci", decode="")
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
     * message.
     * @Column(name="message", type="varchar", length=2048, accuracy=0, nullable=false, default="", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false, unsigned=false, virtual=false)
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
     * @param string|null $message message
     * @return static
     */
    public function setMessage($message)
    {
        $this->message = null === $message ? null : (string)$message;
        return $this;
    }

}
