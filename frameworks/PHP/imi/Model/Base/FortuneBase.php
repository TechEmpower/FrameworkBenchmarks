<?php
declare(strict_types=1);

namespace ImiApp\Model\Base;

use Imi\Model\Model as Model;
use Imi\Model\Annotation\DDL;
use Imi\Model\Annotation\Table;
use Imi\Model\Annotation\Column;
use Imi\Model\Annotation\Entity;

/**
 * fortune 基类
 * @Entity(bean=false)
 * @Table(name="fortune", id={"id"})
 * @DDL(sql="CREATE TABLE `fortune` (   `id` int(10) unsigned NOT NULL AUTO_INCREMENT,   `message` varchar(2048) CHARACTER SET utf8 NOT NULL,   PRIMARY KEY (`id`) ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci", decode="")
 * @property int|null $id 
 * @property string|null $message 
 */
abstract class FortuneBase extends Model
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
     * message
     * @Column(name="message", type="varchar", length=2048, accuracy=0, nullable=false, default="", isPrimaryKey=false, primaryKeyIndex=-1, isAutoIncrement=false)
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
    public function setMessage($message)
    {
        $this->message = null === $message ? null : (string)$message;
        return $this;
    }

}
