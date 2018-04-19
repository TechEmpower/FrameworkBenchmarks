/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.IOException;
import javax.annotation.Priority;
import org.redkale.util.*;

/**
 * 协议拦截器类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <C> Context的子类型
 * @param <R> Request的子类型
 * @param <P> Response的子类型
 */
public abstract class Filter<C extends Context, R extends Request<C>, P extends Response<C, R>> implements Comparable {

    AnyValue _conf; //当前Filter的配置

    Filter<C, R, P> _next; //下一个Filter

    public void init(C context, AnyValue config) {
    }

    public abstract void doFilter(R request, P response) throws IOException;

    public void destroy(C context, AnyValue config) {
    }

    @Override
    public int compareTo(Object o) {
        if (!(o instanceof Filter)) return 1;
        Priority p1 = this.getClass().getAnnotation(Priority.class);
        Priority p2 = o.getClass().getAnnotation(Priority.class);
        return (p2 == null ? 0 : p2.value()) - (p1 == null ? 0 : p1.value());
    }
}
