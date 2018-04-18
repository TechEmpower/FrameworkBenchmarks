/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;

/**
 * 分表分库策略，结合&#64;DistributeTable使用   <br>
 * 不能与&#64;Cacheable同时使用   <br>
 * 使用分表分库功能重点是主键的生成策略，不同场景生成策略不一样   <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> Entity类型
 */
public interface DistributeTableStrategy<T> {

    /**
     * 获取对象的表名   <br>
     * 查询单个对象（DataSource.find）时调用本方法获取表名   <br>
     *
     * @param table   模板表的表名
     * @param primary 记录主键
     *
     * @return 带库名的全表名
     */
    public String getTable(String table, Serializable primary);

    /**
     * 获取对象的表名   <br>
     * 新增对象或更新单个对象（DataSource.insert、DataSource.update）时调用本方法获取表名   <br>
     *
     * @param table 模板表的表名
     * @param bean  实体对象
     *
     * @return 带库名的全表名
     */
    public String getTable(String table, T bean);

    /**
     * 获取对象的表名   <br>
     * 查询、修改、删除对象（DataSource.find、DataSource.query、DataSource.delete、DataSource.update）时调用本方法获取表名   <br>
     * 注意： 需保证FilterNode过滤的结果集合必须在一个数据库表中   <br>
     *
     * @param table 模板表的表名
     * @param node  过滤条件
     *
     * @return 带库名的全表名
     */
    public String getTable(String table, FilterNode node);

}
