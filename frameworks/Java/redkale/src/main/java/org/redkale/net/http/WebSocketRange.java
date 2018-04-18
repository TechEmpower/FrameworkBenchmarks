/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.Serializable;
import java.util.Map;
import org.redkale.convert.json.JsonConvert;

/**
 * WebSocket.broadcastMessage时的过滤条件
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class WebSocketRange implements Serializable {
    
    protected String wskey;
    
    protected Map<String, String> attach;
    
    public WebSocketRange() {
    }
    
    public WebSocketRange(String wskey) {
        this.wskey = wskey;
    }
    
    public WebSocketRange(String wskey, Map<String, String> attach) {
        this.wskey = wskey;
        this.attach = attach;
    }
    
    public String getWskey() {
        return wskey;
    }
    
    public void setWskey(String wskey) {
        this.wskey = wskey;
    }
    
    public Map<String, String> getAttach() {
        return attach;
    }
    
    public void setAttach(Map<String, String> attach) {
        this.attach = attach;
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }
}
