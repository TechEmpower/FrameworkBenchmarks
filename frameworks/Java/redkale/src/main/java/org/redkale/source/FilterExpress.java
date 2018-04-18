/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

/**
 * 函数表达式， 均与SQL定义中的表达式相同
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public enum FilterExpress {

    EQUAL("="),
    IGNORECASEEQUAL("="),//不区分大小写的 =
    NOTEQUAL("<>"),
    IGNORECASENOTEQUAL("="),//不区分大小写的 <>
    GREATERTHAN(">"),
    LESSTHAN("<"),
    GREATERTHANOREQUALTO(">="),
    LESSTHANOREQUALTO("<="),
    STARTSWITH("LIKE"),
    NOTSTARTSWITH("NOT LIKE"),
    ENDSWITH("LIKE"),
    NOTENDSWITH("NOT LIKE"),
    LIKE("LIKE"),
    NOTLIKE("NOT LIKE"),
    IGNORECASELIKE("LIKE"), //不区分大小写的 LIKE
    IGNORECASENOTLIKE("NOT LIKE"), //不区分大小写的 NOT LIKE

    CONTAIN("CONTAIN"), //包含， 相当于反向LIKE 
    NOTCONTAIN("NOT CONTAIN"), //不包含， 相当于反向LIKE 
    IGNORECASECONTAIN("CONTAIN"), //不区分大小写的 CONTAIN
    IGNORECASENOTCONTAIN("NOT CONTAIN"), //不区分大小写的 NOT CONTAIN

    BETWEEN("BETWEEN"),
    NOTBETWEEN("NOT BETWEEN"),
    IN("IN"),
    NOTIN("NOT IN"),
    ISNULL("IS NULL"),
    ISNOTNULL("IS NOT NULL"),
    ISEMPTY("="),//值为空
    ISNOTEMPTY("<>"), //值不为空
    OPAND("&"), //与运算 > 0
    OPOR("|"), //或运算 > 0
    OPANDNO("&"), //与运算 == 0
    FV_MOD("%"), //取模运算，需要与FilterValue配合使用
    FV_DIV("DIV"), //整除运算，需要与FilterValue配合使用
    AND("AND"),
    OR("OR");

    private final String value;

    private FilterExpress(String v) {
        this.value = v;
    }

    public String value() {
        return value;
    }

}
