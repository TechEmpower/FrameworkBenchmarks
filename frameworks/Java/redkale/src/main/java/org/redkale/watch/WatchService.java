/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.watch;

import org.redkale.service.*;

/**
 * 只给WATCH协议的Server才能加载的Service，其他协议的Server均不能自动加载WatchService
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface WatchService extends Service {

}
