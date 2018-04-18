/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import org.redkale.util.AnyValue;
import java.io.IOException;

/**
 * 协议请求处理类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <C> Context的子类型
 * @param <R> Request的子类型
 * @param <P> Response的子类型
 */
public abstract class Servlet<C extends Context, R extends Request<C>, P extends Response<C, R>> {

    AnyValue _conf; //当前Servlet的配置

    public void init(C context, AnyValue config) {
    }

    public abstract void execute(R request, P response) throws IOException;

    public void destroy(C context, AnyValue config) {
    }

}
