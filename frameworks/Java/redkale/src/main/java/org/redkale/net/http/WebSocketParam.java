/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

/**
 *
 * 供WebSocket.preOnMessage 方法获取RestWebSocket里OnMessage方法的参数  <br>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface WebSocketParam {

    public <T> T getValue(String name);
    
    public String[] getNames();
}
