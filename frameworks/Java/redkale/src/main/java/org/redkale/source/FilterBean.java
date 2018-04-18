/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

/**
 * FilterBean用于过滤条件， 所有的FilterBean都必须可以转换成FilterNode  <br>
 *
 * 不被标记为&#64;javax.persistence.Transient 的字段均视为过滤条件   <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface FilterBean {

}
