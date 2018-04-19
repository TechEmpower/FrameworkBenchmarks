/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;
import org.redkale.util.*;

/**
 *
 * DataSource 为数据库或内存数据库的数据源，提供类似JPA、Hibernate的接口与功能。  <br>
 * 返回类型为CompletableFuture的接口为异步接口
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
public interface DataSource {

    /**
     * 获取数据源类型
     *
     * @return String
     */
    public String getType();

    //----------------------insertAsync-----------------------------
    /**
     * 新增记录， 多对象必须是同一个Entity类  <br>
     *
     * @param <T>    泛型
     * @param values Entity对象
     */
    public <T> void insert(final T... values);

    /**
     * 新增记录， 多对象必须是同一个Entity类  <br>
     *
     * @param <T>    泛型
     * @param values Entity对象
     *
     * @return CompletableFuture
     */
    public <T> CompletableFuture<Void> insertAsync(final T... values);

    //-------------------------deleteAsync--------------------------
    /**
     * 删除指定主键值的记录， 多对象必须是同一个Entity类  <br>
     * 等价SQL: DELETE FROM {table} WHERE {primary} IN {values.id}  <br>
     *
     * @param <T>    泛型
     * @param values Entity对象
     *
     * @return 影响的记录条数
     */
    public <T> int delete(final T... values);

    /**
     * 删除指定主键值的记录， 多对象必须是同一个Entity类  <br>
     * 等价SQL: DELETE FROM {table} WHERE {primary} IN {values.id}  <br>
     *
     * @param <T>    泛型
     * @param values Entity对象
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> deleteAsync(final T... values);

    /**
     * 删除指定主键值的记录  <br>
     * 等价SQL: DELETE FROM {table} WHERE {primary} IN {ids}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param ids   主键值
     *
     * @return 影响的记录条数
     */
    public <T> int delete(final Class<T> clazz, final Serializable... ids);

    /**
     * 删除指定主键值的记录  <br>
     * 等价SQL: DELETE FROM {table} WHERE {primary} IN {ids}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param ids   主键值
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> deleteAsync(final Class<T> clazz, final Serializable... ids);

    /**
     * 删除符合过滤条件的记录  <br>
     * 等价SQL: DELETE FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return 影响的记录条数
     */
    public <T> int delete(final Class<T> clazz, final FilterNode node);

    /**
     * 删除符合过滤条件的记录  <br>
     * 等价SQL: DELETE FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> deleteAsync(final Class<T> clazz, final FilterNode node);

    /**
     * 删除符合过滤条件且指定最大影响条数的记录  <br>
     * Flipper中offset字段将被忽略  <br>
     * 等价SQL: DELETE FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return 影响的记录条数
     */
    public <T> int delete(final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 删除符合过滤条件且指定最大影响条数的记录  <br>
     * Flipper中offset字段将被忽略  <br>
     * 等价SQL: DELETE FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> deleteAsync(final Class<T> clazz, final Flipper flipper, final FilterNode node);

    //------------------------updateAsync---------------------------
    /**
     * 更新记录， 多对象必须是同一个Entity类  <br>
     * 等价SQL:  <br>
     * UPDATE {table} SET column1 = value1, column2 = value2, &#183;&#183;&#183; WHERE {primary} = {id1}  <br>
     * UPDATE {table} SET column1 = value1, column2 = value2, &#183;&#183;&#183; WHERE {primary} = {id2}  <br>
     * &#183;&#183;&#183;  <br>
     *
     * @param <T>    泛型
     * @param values Entity对象
     *
     * @return 影响的记录条数
     */
    public <T> int update(final T... values);

    /**
     * 更新记录， 多对象必须是同一个Entity类  <br>
     * 等价SQL:  <br>
     * UPDATE {table} SET column1 = value1, column2 = value2, &#183;&#183;&#183; WHERE {primary} = {id1}  <br>
     * UPDATE {table} SET column1 = value1, column2 = value2, &#183;&#183;&#183; WHERE {primary} = {id2}  <br>
     * &#183;&#183;&#183;  <br>
     *
     * @param <T>    泛型
     * @param values Entity对象
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateAsync(final T... values);

    /**
     * 更新单个记录的单个字段  <br>
     * <b>注意</b>：即使字段标记为&#064;Column(updatable=false)也会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column} = {value} WHERE {primary} = {id}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param id     主键
     * @param column 待更新的字段名
     * @param value  更新值
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final Class<T> clazz, final Serializable id, final String column, final Serializable value);

    /**
     * 更新单个记录的单个字段  <br>
     * <b>注意</b>：即使字段标记为&#064;Column(updatable=false)也会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column} = {value} WHERE {primary} = {id}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param id     主键
     * @param column 待更新的字段名
     * @param value  更新值
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final Class<T> clazz, final Serializable id, final String column, final Serializable value);

    /**
     * 更新符合过滤条件记录的单个字段   <br>
     * <b>注意</b>：即使字段标记为&#064;Column(updatable=false)也会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column} = {value} WHERE {filter node}   <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 待更新的字段名
     * @param value  更新值
     * @param node   过滤条件
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final Class<T> clazz, final String column, final Serializable value, final FilterNode node);

    /**
     * 更新符合过滤条件记录的单个字段   <br>
     * <b>注意</b>：即使字段标记为&#064;Column(updatable=false)也会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column} = {value} WHERE {filter node}   <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 待更新的字段名
     * @param value  更新值
     * @param node   过滤条件
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final Class<T> clazz, final String column, final Serializable value, final FilterNode node);

    /**
     * 更新指定主键值记录的部分字段   <br>
     * 字段赋值操作选项见 ColumnExpress   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} += {value2}, {column3} *= {value3}, &#183;&#183;&#183; WHERE {filter node}   <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param id     主键
     * @param values 更新字段
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final Class<T> clazz, final Serializable id, final ColumnValue... values);

    /**
     * 更新指定主键值记录的部分字段   <br>
     * 字段赋值操作选项见 ColumnExpress   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} += {value2}, {column3} *= {value3}, &#183;&#183;&#183; WHERE {filter node}   <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param id     主键
     * @param values 更新字段
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final Class<T> clazz, final Serializable id, final ColumnValue... values);

    /**
     * 更新符合过滤条件记录的部分字段   <br>
     * 字段赋值操作选项见 ColumnExpress   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} += {value2}, {column3} *= {value3}, &#183;&#183;&#183; WHERE {filter node}   <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param node   过滤条件
     * @param values 更新字段
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final Class<T> clazz, final FilterNode node, final ColumnValue... values);

    /**
     * 更新符合过滤条件记录的部分字段   <br>
     * 字段赋值操作选项见 ColumnExpress   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} += {value2}, {column3} *= {value3}, &#183;&#183;&#183; WHERE {filter node}   <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param node   过滤条件
     * @param values 更新字段
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final Class<T> clazz, final FilterNode node, final ColumnValue... values);

    /**
     * 更新符合过滤条件的记录的指定字段   <br>
     * Flipper中offset字段将被忽略   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} += {value2}, {column3} *= {value3}, &#183;&#183;&#183; WHERE {filter node} ORDER BY {flipper.sort}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param node    过滤条件
     * @param flipper 翻页对象
     * @param values  更新字段
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final Class<T> clazz, final FilterNode node, final Flipper flipper, final ColumnValue... values);

    /**
     * 更新符合过滤条件的记录的指定字段   <br>
     * Flipper中offset字段将被忽略   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} += {value2}, {column3} *= {value3}, &#183;&#183;&#183; WHERE {filter node} ORDER BY {flipper.sort}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param node    过滤条件
     * @param flipper 翻页对象
     * @param values  更新字段
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final Class<T> clazz, final FilterNode node, final Flipper flipper, final ColumnValue... values);

    /**
     * 更新单个记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {primary} = {bean.id}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param columns 需更新的字段名
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final T bean, final String... columns);

    /**
     * 更新单个记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {primary} = {bean.id}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param columns 需更新的字段名
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final T bean, final String... columns);

    /**
     * 更新符合过滤条件记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param node    过滤条件
     * @param columns 需更新的字段名
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final T bean, final FilterNode node, final String... columns);

    /**
     * 更新符合过滤条件记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param node    过滤条件
     * @param columns 需更新的字段名
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final T bean, final FilterNode node, final String... columns);

    /**
     * 更新单个记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {primary} = {bean.id}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param selects 指定字段
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final T bean, final SelectColumn selects);

    /**
     * 更新单个记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {primary} = {bean.id}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param selects 指定字段
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final T bean, final SelectColumn selects);

    /**
     * 更新符合过滤条件记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param node    过滤条件
     * @param selects 指定字段
     *
     * @return 影响的记录条数
     */
    public <T> int updateColumn(final T bean, final FilterNode node, final SelectColumn selects);

    /**
     * 更新符合过滤条件记录的指定字段   <br>
     * <b>注意</b>：Entity类中标记为&#064;Column(updatable=false)不会被更新   <br>
     * 等价SQL: UPDATE {table} SET {column1} = {value1}, {column2} = {value2}, {column3} = {value3}, &#183;&#183;&#183; WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param bean    待更新的Entity对象
     * @param node    过滤条件
     * @param selects 指定字段
     *
     * @return 影响的记录条数CompletableFuture
     */
    public <T> CompletableFuture<Integer> updateColumnAsync(final T bean, final FilterNode node, final SelectColumn selects);

    //############################################# 查询接口 #############################################
    //-----------------------getXXXXResult-----------------------------
    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回null   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.COUNT, null) 等价于: SELECT COUNT(*) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param column      指定字段
     *
     * @return 聚合结果
     */
    public Number getNumberResult(final Class entityClass, final FilterFunc func, final String column);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回null   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.COUNT, null) 等价于: SELECT COUNT(*) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param column      指定字段
     *
     * @return 聚合结果CompletableFuture
     */
    public CompletableFuture<Number> getNumberResultAsync(final Class entityClass, final FilterFunc func, final String column);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回null   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter bean}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.COUNT, null, (FilterBean)null) 等价于: SELECT COUNT(*) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param column      指定字段
     * @param bean        过滤条件
     *
     * @return 聚合结果
     */
    public Number getNumberResult(final Class entityClass, final FilterFunc func, final String column, final FilterBean bean);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回null   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter bean}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.COUNT, null, (FilterBean)null) 等价于: SELECT COUNT(*) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param column      指定字段
     * @param bean        过滤条件
     *
     * @return 聚合结果CompletableFuture
     */
    public CompletableFuture<Number> getNumberResultAsync(final Class entityClass, final FilterFunc func, final String column, final FilterBean bean);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回null   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter node}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param column      指定字段
     * @param node        过滤条件
     *
     * @return 聚合结果
     */
    public Number getNumberResult(final Class entityClass, final FilterFunc func, final String column, final FilterNode node);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回null   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter node}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param column      指定字段
     * @param node        过滤条件
     *
     * @return 聚合结果
     */
    public CompletableFuture<Number> getNumberResultAsync(final Class entityClass, final FilterFunc func, final String column, final FilterNode node);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回默认值   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime") 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param defVal      默认值
     * @param column      指定字段
     *
     * @return 聚合结果
     */
    public Number getNumberResult(final Class entityClass, final FilterFunc func, final Number defVal, final String column);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回默认值   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime") 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param defVal      默认值
     * @param column      指定字段
     *
     * @return 聚合结果CompletableFuture
     */
    public CompletableFuture<Number> getNumberResultAsync(final Class entityClass, final FilterFunc func, final Number defVal, final String column);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回默认值   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter bean}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param defVal      默认值
     * @param column      指定字段
     * @param bean        过滤条件
     *
     * @return 聚合结果
     */
    public Number getNumberResult(final Class entityClass, final FilterFunc func, final Number defVal, final String column, final FilterBean bean);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回默认值   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter bean}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param defVal      默认值
     * @param column      指定字段
     * @param bean        过滤条件
     *
     * @return 聚合结果CompletableFuture
     */
    public CompletableFuture<Number> getNumberResultAsync(final Class entityClass, final FilterFunc func, final Number defVal, final String column, final FilterBean bean);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回默认值   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter node}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param defVal      默认值
     * @param column      指定字段
     * @param node        过滤条件
     *
     * @return 聚合结果
     */
    public Number getNumberResult(final Class entityClass, final FilterFunc func, final Number defVal, final String column, final FilterNode node);

    /**
     * 获取符合过滤条件记录的聚合结果, 无结果返回默认值   <br>
     * 等价SQL: SELECT FUNC{column} FROM {table} WHERE {filter node}  <br>
     * 如 getNumberResultAsync(Record.class, FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param entityClass Entity类
     * @param func        聚合函数
     * @param defVal      默认值
     * @param column      指定字段
     * @param node        过滤条件
     *
     * @return 聚合结果CompletableFuture
     */
    public CompletableFuture<Number> getNumberResultAsync(final Class entityClass, final FilterFunc func, final Number defVal, final String column, final FilterNode node);

    /**
     * 获取符合过滤条件记录的聚合结果Map   <br>
     * 等价SQL: SELECT FUNC1{column1}, FUNC2{column2}, &#183;&#183;&#183; FROM {table}  <br>
     * 如 getNumberMapAsync(Record.class, new FilterFuncColumn(FilterFunc.MAX, "createtime")) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param <N>         Number
     * @param entityClass Entity类
     * @param columns     聚合字段
     *
     * @return 聚合结果Map
     */
    public <N extends Number> Map<String, N> getNumberMap(final Class entityClass, final FilterFuncColumn... columns);

    /**
     * 获取符合过滤条件记录的聚合结果Map   <br>
     * 等价SQL: SELECT FUNC1{column1}, FUNC2{column2}, &#183;&#183;&#183; FROM {table}  <br>
     * 如 getNumberMapAsync(Record.class, new FilterFuncColumn(FilterFunc.MAX, "createtime")) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param <N>         Number
     * @param entityClass Entity类
     * @param columns     聚合字段
     *
     * @return 聚合结果Map CompletableFuture
     */
    public <N extends Number> CompletableFuture<Map<String, N>> getNumberMapAsync(final Class entityClass, final FilterFuncColumn... columns);

    /**
     * 获取符合过滤条件记录的聚合结果Map   <br>
     * 等价SQL: SELECT FUNC1{column1}, FUNC2{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean}  <br>
     * 如 getNumberMapAsync(Record.class, (FilterBean)null, new FilterFuncColumn(FilterFunc.MAX, "createtime")) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param <N>         Number
     * @param entityClass Entity类
     * @param bean        过滤条件
     * @param columns     聚合字段
     *
     * @return 聚合结果Map
     */
    public <N extends Number> Map<String, N> getNumberMap(final Class entityClass, final FilterBean bean, final FilterFuncColumn... columns);

    /**
     * 获取符合过滤条件记录的聚合结果Map   <br>
     * 等价SQL: SELECT FUNC1{column1}, FUNC2{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean}  <br>
     * 如 getNumberMapAsync(Record.class, (FilterBean)null, new FilterFuncColumn(FilterFunc.MAX, "createtime")) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param <N>         Number
     * @param entityClass Entity类
     * @param bean        过滤条件
     * @param columns     聚合字段
     *
     * @return 聚合结果Map CompletableFuture
     */
    public <N extends Number> CompletableFuture<Map<String, N>> getNumberMapAsync(final Class entityClass, final FilterBean bean, final FilterFuncColumn... columns);

    /**
     * 获取符合过滤条件记录的聚合结果Map   <br>
     * 等价SQL: SELECT FUNC1{column1}, FUNC2{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node}  <br>
     * 如 getNumberMapAsync(Record.class, (FilterNode)null, new FilterFuncColumn(FilterFunc.MAX, "createtime")) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param <N>         Number
     * @param entityClass Entity类
     * @param node        过滤条件
     * @param columns     聚合字段
     *
     * @return 聚合结果Map
     */
    public <N extends Number> Map<String, N> getNumberMap(final Class entityClass, final FilterNode node, final FilterFuncColumn... columns);

    /**
     * 获取符合过滤条件记录的聚合结果Map   <br>
     * 等价SQL: SELECT FUNC1{column1}, FUNC2{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node}  <br>
     * 如 getNumberMapAsync(Record.class, (FilterNode)null, new FilterFuncColumn(FilterFunc.MAX, "createtime")) 等价于: SELECT MAX(createtime) FROM {table} <br>
     *
     * @param <N>         Number
     * @param entityClass Entity类
     * @param node        过滤条件
     * @param columns     聚合字段
     *
     * @return 聚合结果Map
     */
    public <N extends Number> CompletableFuture<Map<String, N>> getNumberMapAsync(final Class entityClass, final FilterNode node, final FilterFuncColumn... columns);

    /**
     * 查询符合过滤条件记录的GROUP BY聚合结果Map   <br>
     * 等价SQL: SELECT keyColumn, FUNC{funcColumn} FROM {table} GROUP BY {keyColumn}  <br>
     * 如 queryColumnMapAsync(Record.class, "name", FilterFunc.MAX, "createtime") 等价于: SELECT name, MAX(createtime) FROM record GROUP BY name<br>
     *
     * @param <T>         Entity泛型
     * @param <K>         Key字段的数据类型
     * @param <N>         Number
     * @param entityClass Entity类
     * @param keyColumn   Key字段
     * @param func        聚合函数
     * @param funcColumn  聚合字段
     *
     * @return 聚合结果Map
     */
    public <T, K extends Serializable, N extends Number> Map<K, N> queryColumnMap(final Class<T> entityClass, final String keyColumn, final FilterFunc func, final String funcColumn);

    /**
     * 查询符合过滤条件记录的GROUP BY聚合结果Map   <br>
     * 等价SQL: SELECT keyColumn, FUNC{funcColumn} FROM {table} GROUP BY {keyColumn}  <br>
     * 如 queryColumnMapAsync(Record.class, "name", FilterFunc.MAX, "createtime") 等价于: SELECT name, MAX(createtime) FROM record GROUP BY name<br>
     *
     * @param <T>         Entity泛型
     * @param <K>         Key字段的数据类型
     * @param <N>         Number
     * @param entityClass Entity类
     * @param keyColumn   Key字段
     * @param func        聚合函数
     * @param funcColumn  聚合字段
     *
     * @return 聚合结果Map CompletableFuture
     */
    public <T, K extends Serializable, N extends Number> CompletableFuture<Map<K, N>> queryColumnMapAsync(final Class<T> entityClass, final String keyColumn, final FilterFunc func, final String funcColumn);

    /**
     * 查询符合过滤条件记录的GROUP BY聚合结果Map   <br>
     * 等价SQL: SELECT keyColumn, FUNC{funcColumn} FROM {table} WHERE {filter bean} GROUP BY {keyColumn}  <br>
     * 如 queryColumnMapAsync(Record.class, "name", FilterFunc.MAX, "createtime", (FilterBean)null) 等价于: SELECT name, MAX(createtime) FROM record GROUP BY name<br>
     *
     * @param <T>         Entity泛型
     * @param <K>         Key字段的数据类型
     * @param <N>         Number
     * @param entityClass Entity类
     * @param keyColumn   Key字段
     * @param func        聚合函数
     * @param funcColumn  聚合字段
     * @param bean        过滤条件
     *
     * @return 聚合结果Map
     */
    public <T, K extends Serializable, N extends Number> Map<K, N> queryColumnMap(final Class<T> entityClass, final String keyColumn, final FilterFunc func, final String funcColumn, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的GROUP BY聚合结果Map   <br>
     * 等价SQL: SELECT keyColumn, FUNC{funcColumn} FROM {table} WHERE {filter bean} GROUP BY {keyColumn}  <br>
     * 如 queryColumnMapAsync(Record.class, "name", FilterFunc.MAX, "createtime", (FilterBean)null) 等价于: SELECT name, MAX(createtime) FROM record GROUP BY name<br>
     *
     * @param <T>         Entity泛型
     * @param <K>         Key字段的数据类型
     * @param <N>         Number
     * @param entityClass Entity类
     * @param keyColumn   Key字段
     * @param func        聚合函数
     * @param funcColumn  聚合字段
     * @param bean        过滤条件
     *
     * @return 聚合结果Map CompletableFuture
     */
    public <T, K extends Serializable, N extends Number> CompletableFuture<Map<K, N>> queryColumnMapAsync(final Class<T> entityClass, final String keyColumn, final FilterFunc func, final String funcColumn, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的GROUP BY聚合结果Map   <br>
     * 等价SQL: SELECT keyColumn, FUNC{funcColumn} FROM {table} WHERE {filter node} GROUP BY {keyColumn}  <br>
     * 如 queryColumnMapAsync(Record.class, "name", FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT name, MAX(createtime) FROM record GROUP BY name<br>
     *
     * @param <T>         Entity泛型
     * @param <K>         Key字段的数据类型
     * @param <N>         Number
     * @param entityClass Entity类
     * @param keyColumn   Key字段
     * @param func        聚合函数
     * @param funcColumn  聚合字段
     * @param node        过滤条件
     *
     * @return 聚合结果Map
     */
    public <T, K extends Serializable, N extends Number> Map<K, N> queryColumnMap(final Class<T> entityClass, final String keyColumn, final FilterFunc func, final String funcColumn, final FilterNode node);

    /**
     * 查询符合过滤条件记录的GROUP BY聚合结果Map   <br>
     * 等价SQL: SELECT keyColumn, FUNC{funcColumn} FROM {table} WHERE {filter node} GROUP BY {keyColumn}  <br>
     * 如 queryColumnMapAsync(Record.class, "name", FilterFunc.MAX, "createtime", (FilterNode)null) 等价于: SELECT name, MAX(createtime) FROM record GROUP BY name<br>
     *
     * @param <T>         Entity泛型
     * @param <K>         Key字段的数据类型
     * @param <N>         Number
     * @param entityClass Entity类
     * @param keyColumn   Key字段
     * @param func        聚合函数
     * @param funcColumn  聚合字段
     * @param node        过滤条件
     *
     * @return 聚合结果Map CompletableFuture
     */
    public <T, K extends Serializable, N extends Number> CompletableFuture<Map<K, N>> queryColumnMapAsync(final Class<T> entityClass, final String keyColumn, final FilterFunc func, final String funcColumn, final FilterNode node);

    //-----------------------findAsync----------------------------
    /**
     * 获取指定主键值的单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param pk    主键值
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final Serializable pk);

    /**
     * 获取指定主键值的单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param pk    主键值
     *
     * @return Entity对象 CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final Serializable pk);

    /**
     * 获取指定主键值的单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param pk      主键值
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final SelectColumn selects, final Serializable pk);

    /**
     * 获取指定主键值的单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param pk      主键值
     *
     * @return Entity对象CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final SelectColumn selects, final Serializable pk);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 过滤字段名
     * @param key    过滤字段值
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final String column, final Serializable key);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 过滤字段名
     * @param key    过滤字段值
     *
     * @return Entity对象CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final String column, final Serializable key);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  过滤条件
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  过滤条件
     *
     * @return Entity对象CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return Entity对象CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param bean    过滤条件
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final SelectColumn selects, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param bean    过滤条件
     *
     * @return Entity对象 CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final SelectColumn selects, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param node    过滤条件
     *
     * @return Entity对象
     */
    public <T> T find(final Class<T> clazz, final SelectColumn selects, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param node    过滤条件
     *
     * @return Entity对象 CompletableFuture
     */
    public <T> CompletableFuture<T> findAsync(final Class<T> clazz, final SelectColumn selects, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 字段名
     * @param pk     主键值
     *
     * @return Entity对象
     */
    public <T> Serializable findColumn(final Class<T> clazz, final String column, final Serializable pk);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 字段名
     * @param pk     主键值
     *
     * @return Entity对象 CompletableFuture
     */
    public <T> CompletableFuture<Serializable> findColumnAsync(final Class<T> clazz, final String column, final Serializable pk);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 字段名
     * @param bean   过滤条件
     *
     * @return 字段值
     */
    public <T> Serializable findColumn(final Class<T> clazz, final String column, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 字段名
     * @param bean   过滤条件
     *
     * @return 字段值 CompletableFuture
     */
    public <T> CompletableFuture<Serializable> findColumnAsync(final Class<T> clazz, final String column, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 字段名
     * @param node   过滤条件
     *
     * @return 字段值
     */
    public <T> Serializable findColumn(final Class<T> clazz, final String column, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 返回null表示不存在值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 字段名
     * @param node   过滤条件
     *
     * @return 字段值 CompletableFuture
     */
    public <T> CompletableFuture<Serializable> findColumnAsync(final Class<T> clazz, final String column, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 不存在值则返回默认值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>      Entity泛型
     * @param clazz    Entity类
     * @param column   字段名
     * @param defValue 默认值
     * @param pk       主键值
     *
     * @return 字段值
     */
    public <T> Serializable findColumn(final Class<T> clazz, final String column, final Serializable defValue, final Serializable pk);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 不存在值则返回默认值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>      Entity泛型
     * @param clazz    Entity类
     * @param column   字段名
     * @param defValue 默认值
     * @param pk       主键值
     *
     * @return 字段值 CompletableFuture
     */
    public <T> CompletableFuture<Serializable> findColumnAsync(final Class<T> clazz, final String column, final Serializable defValue, final Serializable pk);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 不存在值则返回默认值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>      Entity泛型
     * @param clazz    Entity类
     * @param column   字段名
     * @param defValue 默认值
     * @param bean     过滤条件
     *
     * @return 字段值
     */
    public <T> Serializable findColumn(final Class<T> clazz, final String column, final Serializable defValue, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 不存在值则返回默认值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>      Entity泛型
     * @param clazz    Entity类
     * @param column   字段名
     * @param defValue 默认值
     * @param bean     过滤条件
     *
     * @return 字段值 CompletableFuture
     */
    public <T> CompletableFuture<Serializable> findColumnAsync(final Class<T> clazz, final String column, final Serializable defValue, final FilterBean bean);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 不存在值则返回默认值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>      Entity泛型
     * @param clazz    Entity类
     * @param column   字段名
     * @param defValue 默认值
     * @param node     过滤条件
     *
     * @return 字段值
     */
    public <T> Serializable findColumn(final Class<T> clazz, final String column, final Serializable defValue, final FilterNode node);

    /**
     * 获取符合过滤条件单个记录的单个字段值, 不存在值则返回默认值   <br>
     * 等价SQL: SELECT {column} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>      Entity泛型
     * @param clazz    Entity类
     * @param column   字段名
     * @param defValue 默认值
     * @param node     过滤条件
     *
     * @return 字段值 CompletableFuture
     */
    public <T> CompletableFuture<Serializable> findColumnAsync(final Class<T> clazz, final String column, final Serializable defValue, final FilterNode node);

    /**
     * 判断是否存在主键值的记录   <br>
     * 等价SQL: SELECT COUNT(*) FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param pk    主键值
     *
     * @return 是否存在
     */
    public <T> boolean exists(final Class<T> clazz, final Serializable pk);

    /**
     * 判断是否存在主键值的记录   <br>
     * 等价SQL: SELECT COUNT(*) FROM {table} WHERE {primary} = {id}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param pk    主键值
     *
     * @return 是否存在CompletableFuture
     */
    public <T> CompletableFuture<Boolean> existsAsync(final Class<T> clazz, final Serializable pk);

    /**
     * 判断是否存在符合过滤条件的记录   <br>
     * 等价SQL: SELECT COUNT(*) FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  过滤条件
     *
     * @return 是否存在
     */
    public <T> boolean exists(final Class<T> clazz, final FilterBean bean);

    /**
     * 判断是否存在符合过滤条件的记录   <br>
     * 等价SQL: SELECT COUNT(*) FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  过滤条件
     *
     * @return 是否存在CompletableFuture
     */
    public <T> CompletableFuture<Boolean> existsAsync(final Class<T> clazz, final FilterBean bean);

    /**
     * 判断是否存在符合过滤条件的记录   <br>
     * 等价SQL: SELECT COUNT(*) FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return 是否存在
     */
    public <T> boolean exists(final Class<T> clazz, final FilterNode node);

    /**
     * 判断是否存在符合过滤条件的记录   <br>
     * 等价SQL: SELECT COUNT(*) FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return 是否存在CompletableFuture
     */
    public <T> CompletableFuture<Boolean> existsAsync(final Class<T> clazz, final FilterNode node);

    //-----------------------list set----------------------------
    /**
     * 查询符合过滤条件记录的某个字段Set集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {column} = {key}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param column         过滤字段名
     * @param key            过滤字段值
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> HashSet<V> queryColumnSet(final String selectedColumn, final Class<T> clazz, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的某个字段Set集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {column} = {key}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param column         过滤字段名
     * @param key            过滤字段值
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<HashSet<V>> queryColumnSetAsync(final String selectedColumn, final Class<T> clazz, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的某个字段Set集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param bean           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> HashSet<V> queryColumnSet(final String selectedColumn, final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段Set集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param bean           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<HashSet<V>> queryColumnSetAsync(final String selectedColumn, final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段Set集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param node           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> HashSet<V> queryColumnSet(final String selectedColumn, final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段Set集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param node           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<HashSet<V>> queryColumnSetAsync(final String selectedColumn, final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {column} = {key}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param column         过滤字段名
     * @param key            过滤字段值
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> List<V> queryColumnList(final String selectedColumn, final Class<T> clazz, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {column} = {key}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param column         过滤字段名
     * @param key            过滤字段值
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<List<V>> queryColumnListAsync(final String selectedColumn, final Class<T> clazz, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param bean           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> List<V> queryColumnList(final String selectedColumn, final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param bean           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<List<V>> queryColumnListAsync(final String selectedColumn, final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param node           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> List<V> queryColumnList(final String selectedColumn, final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param node           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<List<V>> queryColumnListAsync(final String selectedColumn, final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param bean           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> List<V> queryColumnList(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param bean           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<List<V>> queryColumnListAsync(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param node           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> List<V> queryColumnList(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段List集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param node           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<List<V>> queryColumnListAsync(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段Sheet集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param bean           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> Sheet<V> queryColumnSheet(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段Sheet集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param bean           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<Sheet<V>> queryColumnSheetAsync(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的某个字段Sheet集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param node           过滤条件
     *
     * @return 字段值的集合
     */
    public <T, V extends Serializable> Sheet<V> queryColumnSheet(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的某个字段Sheet集合   <br>
     * 等价SQL: SELECT {selectedColumn} FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>            Entity泛型
     * @param <V>            字段类型
     * @param selectedColumn 指定字段
     * @param clazz          Entity类
     * @param flipper        翻页对象
     * @param node           过滤条件
     *
     * @return 字段值的集合CompletableFuture
     */
    public <T, V extends Serializable> CompletableFuture<Sheet<V>> queryColumnSheetAsync(final String selectedColumn, final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的Map集合, 主键值为key   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>       主键泛型
     * @param <T>       Entity泛型
     * @param clazz     Entity类
     * @param keyStream 主键Stream
     *
     * @return Entity的集合
     */
    public <K extends Serializable, T> Map<K, T> queryMap(final Class<T> clazz, final Stream<K> keyStream);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>       主键泛型
     * @param <T>       Entity泛型
     * @param clazz     Entity类
     * @param keyStream 主键Stream
     *
     * @return Entity的集合CompletableFuture
     */
    public <K extends Serializable, T> CompletableFuture<Map<K, T>> queryMapAsync(final Class<T> clazz, final Stream<K> keyStream);

    /**
     * 查询符合过滤条件记录的Map集合, 主键值为key   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>   主键泛型
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  FilterBean
     *
     * @return Entity的集合
     */
    public <K extends Serializable, T> Map<K, T> queryMap(final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>   主键泛型
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  FilterBean
     *
     * @return Entity的集合CompletableFuture
     */
    public <K extends Serializable, T> CompletableFuture<Map<K, T>> queryMapAsync(final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的Map集合, 主键值为key   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>   主键泛型
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  FilterNode
     *
     * @return Entity的集合
     */
    public <K extends Serializable, T> Map<K, T> queryMap(final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>   主键泛型
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  FilterNode
     *
     * @return Entity的集合CompletableFuture
     */
    public <K extends Serializable, T> CompletableFuture<Map<K, T>> queryMapAsync(final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的Map集合, 主键值为key   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>       主键泛型
     * @param <T>       Entity泛型
     * @param clazz     Entity类
     * @param selects   指定字段
     * @param keyStream 主键Stream
     *
     * @return Entity的集合
     */
    public <K extends Serializable, T> Map<K, T> queryMap(final Class<T> clazz, final SelectColumn selects, final Stream<K> keyStream);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>       主键泛型
     * @param <T>       Entity泛型
     * @param clazz     Entity类
     * @param selects   指定字段
     * @param keyStream 主键Stream
     *
     * @return Entity的集合CompletableFuture
     */
    public <K extends Serializable, T> CompletableFuture<Map<K, T>> queryMapAsync(final Class<T> clazz, final SelectColumn selects, final Stream<K> keyStream);

    /**
     * 查询符合过滤条件记录的Map集合, 主键值为key   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>     主键泛型
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param bean    FilterBean
     *
     * @return Entity的集合
     */
    public <K extends Serializable, T> Map<K, T> queryMap(final Class<T> clazz, final SelectColumn selects, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>     主键泛型
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param bean    FilterBean
     *
     * @return Entity的集合CompletableFuture
     */
    public <K extends Serializable, T> CompletableFuture<Map<K, T>> queryMapAsync(final Class<T> clazz, final SelectColumn selects, FilterBean bean);

    /**
     * 查询符合过滤条件记录的Map集合, 主键值为key   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>     主键泛型
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param node    FilterNode
     *
     * @return Entity的集合
     */
    public <K extends Serializable, T> Map<K, T> queryMap(final Class<T> clazz, final SelectColumn selects, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <K>     主键泛型
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param node    FilterNode
     *
     * @return Entity的集合CompletableFuture
     */
    public <K extends Serializable, T> CompletableFuture<Map<K, T>> queryMapAsync(final Class<T> clazz, final SelectColumn selects, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 过滤字段名
     * @param key    过滤字段值
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>    Entity泛型
     * @param clazz  Entity类
     * @param column 过滤字段名
     * @param key    过滤字段值
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param bean  过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final FilterBean bean);

    /**
     * 查询记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     *
     * @return Entity的集合
     */
    default <T> List<T> queryList(final Class<T> clazz) {
        return queryList(clazz, (FilterNode) null);
    }

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final FilterNode node);

    /**
     * 查询记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     *
     * @return Entity的集合CompletableFuture
     */
    default <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz) {
        return queryListAsync(clazz, (FilterNode) null);
    }

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>   Entity泛型
     * @param clazz Entity类
     * @param node  过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param bean    过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final SelectColumn selects, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param bean    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final SelectColumn selects, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param node    过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final SelectColumn selects, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param node    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final SelectColumn selects, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param column  过滤字段名
     * @param key     过滤字段值
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final Flipper flipper, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {column} = {key} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param column  过滤字段名
     * @param key     过滤字段值
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final Flipper flipper, final String column, final Serializable key);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合
     *
     */
    public <T> List<T> queryList(final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合
     *
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合
     */
    public <T> List<T> queryList(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的List集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<List<T>> queryListAsync(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterNode node);

    //-----------------------sheet----------------------------
    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合
     */
    public <T> Sheet<T> querySheet(final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<Sheet<T>> querySheetAsync(final Class<T> clazz, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合
     */
    public <T> Sheet<T> querySheet(final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT * FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<Sheet<T>> querySheetAsync(final Class<T> clazz, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合
     */
    public <T> Sheet<T> querySheet(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter bean} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param bean    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<Sheet<T>> querySheetAsync(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterBean bean);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合
     */
    public <T> Sheet<T> querySheet(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterNode node);

    /**
     * 查询符合过滤条件记录的Sheet集合   <br>
     * 等价SQL: SELECT {column1},{column2}, &#183;&#183;&#183; FROM {table} WHERE {filter node} ORDER BY {flipper.sort} LIMIT {flipper.limit}  <br>
     *
     * @param <T>     Entity泛型
     * @param clazz   Entity类
     * @param selects 指定字段
     * @param flipper 翻页对象
     * @param node    过滤条件
     *
     * @return Entity的集合CompletableFuture
     */
    public <T> CompletableFuture<Sheet<T>> querySheetAsync(final Class<T> clazz, final SelectColumn selects, final Flipper flipper, final FilterNode node);

}
