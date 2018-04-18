/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.util.*;
import java.util.function.*;
import java.util.regex.*;

/**
 * 判断字符串数组是否包含或排除指定字符串的操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class SelectColumn implements Predicate<String> {

    private Pattern[] patterns;

    private String[] columns;

    private boolean excludable; //是否排除

    public SelectColumn() {
    }

    protected SelectColumn(final String[] columns0, final boolean excludable) {
        this.excludable = excludable;
        final int len = columns0.length;
        if (len < 1) return;
        Pattern[] regs = null;
        String[] cols = null;
        int regcount = 0;
        int colcount = 0;
        for (String col : columns0) {
            boolean reg = false;
            for (int i = 0; i < col.length(); i++) {
                char ch = col.charAt(i);
                if (ch == '^' || ch == '$' || ch == '*' || ch == '?' || ch == '+' || ch == '[' || ch == '(') {
                    reg = true;
                    break;
                }
            }
            if (reg) {
                if (regs == null) regs = new Pattern[len];
                regs[regcount++] = Pattern.compile(col);
            } else {
                if (cols == null) cols = new String[len];
                cols[colcount++] = col;
            }
        }
        if (regs != null) {
            if (regcount == len) {
                this.patterns = regs;
            } else {
                this.patterns = Arrays.copyOf(regs, regcount);
            }
        }
        if (cols != null) {
            if (colcount == len) {
                this.columns = cols;
            } else {
                this.columns = Arrays.copyOf(cols, colcount);
            }
        }
    }

    /**
     * class中的字段名
     *
     * @param columns 包含的字段名集合
     *
     * @return SelectColumn
     */
    public static SelectColumn createIncludes(String... columns) {
        return new SelectColumn(columns, false);
    }

    /**
     * class中的字段名
     *
     * @param cols    包含的字段名集合
     * @param columns 包含的字段名集合
     *
     * @return SelectColumn
     */
    public static SelectColumn createIncludes(String[] cols, String... columns) {
        return new SelectColumn(Utility.append(cols, columns), false);
    }

    /**
     * class中的字段名
     *
     * @param columns 排除的字段名集合
     *
     * @return SelectColumn
     */
    public static SelectColumn createExcludes(String... columns) {
        return new SelectColumn(columns, true);
    }

    /**
     * class中的字段名
     *
     * @param cols    排除的字段名集合
     * @param columns 排除的字段名集合
     *
     * @return SelectColumn
     */
    public static SelectColumn createExcludes(String[] cols, String... columns) {
        return new SelectColumn(Utility.append(cols, columns), true);
    }

    @Override
    public boolean test(final String column) {
        if (this.columns != null) {
            for (String col : this.columns) {
                if (col.equalsIgnoreCase(column)) return !excludable;
            }
        }
        if (this.patterns != null) {
            for (Pattern reg : this.patterns) {
                if (reg.matcher(column).find()) return !excludable;
            }
        }
        return excludable;
    }

    public String[] getColumns() {
        return columns;
    }

    public void setColumns(String[] columns) {
        this.columns = columns;
    }

    public boolean isExcludable() {
        return excludable;
    }

    public void setExcludable(boolean excludable) {
        this.excludable = excludable;
    }

    public Pattern[] getPatterns() {
        return patterns;
    }

    public void setPatterns(Pattern[] patterns) {
        this.patterns = patterns;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getSimpleName()).append("{excludable=").append(excludable);
        if (columns != null) sb.append(", columns=").append(Arrays.toString(columns));
        if (patterns != null) sb.append(", patterns=").append(Arrays.toString(patterns));
        return sb.append('}').toString();
    }

}
