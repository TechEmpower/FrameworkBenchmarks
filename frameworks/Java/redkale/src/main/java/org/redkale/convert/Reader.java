/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

/**
 * 反序列化的数据读取流
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public abstract class Reader {

    //当前对象字段名的游标
    protected int fieldIndex;

    public static final short SIGN_NULL = -1;

    public static final short SIGN_NOLENGTH = -2;

    /**
     * 是否还存在下个元素或字段
     *
     * @return 是否还存在下个元素或字段
     */
    public abstract boolean hasNext();

    /**
     * 跳过值(不包含值前面的字段)
     */
    public abstract void skipValue();

    /**
     * /跳过字段与值之间的多余内容， json就是跳过:符, map跳过:
     */
    public abstract void readBlank();

    /**
     * 读取对象的类名， 返回 null 表示对象为null， 返回空字符串表示当前class与返回的class一致，返回非空字符串表示class是当前class的子类。
     *
     * @param clazz 类名
     *
     * @return 返回字段数
     */
    public String readObjectB(final Class clazz) {
        this.fieldIndex = 0;
        return null;
    }

    /**
     * 读取对象的尾端
     *
     * @param clazz 类名
     */
    public abstract void readObjectE(final Class clazz);

    /**
     * 读取数组的开头并返回数组的长度
     *
     * @return 返回数组的长度
     */
    public abstract int readArrayB();

    /**
     * 读取数组的尾端
     *
     */
    public abstract void readArrayE();

    /**
     * 读取map的开头并返回map的size
     *
     * @return 返回map的size
     */
    public abstract int readMapB();

    /**
     * 读取数组的尾端
     *
     */
    public abstract void readMapE();

    /**
     * 根据字段读取字段对应的DeMember
     *
     * @param members DeMember的全量集合
     *
     * @return 匹配的DeMember
     */
    public abstract DeMember readFieldName(final DeMember[] members);

    /**
     * 读取一个boolean值
     *
     * @return boolean值
     */
    public abstract boolean readBoolean();

    /**
     * 读取一个byte值
     *
     * @return byte值
     */
    public abstract byte readByte();

    /**
     * 读取一个char值
     *
     * @return char值
     */
    public abstract char readChar();

    /**
     * 读取一个short值
     *
     * @return short值
     */
    public abstract short readShort();

    /**
     * 读取一个int值
     *
     * @return int值
     */
    public abstract int readInt();

    /**
     * 读取一个long值
     *
     * @return long值
     */
    public abstract long readLong();

    /**
     * 读取一个float值
     *
     * @return float值
     */
    public abstract float readFloat();

    /**
     * 读取一个double值
     *
     * @return double值
     */
    public abstract double readDouble();

    /**
     * 读取无转义字符长度不超过255的字符串， 例如枚举值、字段名、类名字符串等
     *
     * @return String值
     */
    public abstract String readSmallString();

    /**
     * 读取反解析对象的类名
     *
     * @return 类名
     */
    public abstract String readClassName();

    /**
     * 读取一个String值
     *
     * @return String值
     */
    public abstract String readString();

}
