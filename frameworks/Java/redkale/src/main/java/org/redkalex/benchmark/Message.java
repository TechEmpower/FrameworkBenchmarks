/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import org.redkale.convert.ConvertSmallString;
import org.redkale.convert.json.JsonConvert;
import org.redkale.util.Bean;

/**
 *
 * @author zhangjx
 */
@Bean
public final class Message {

    private static final Message instance = new Message();

    @ConvertSmallString
    private String message;

    public Message() {
    }

    public Message(String message) {
        this.message = message;
    }

    public static Message create(String str) {
        instance.message = str;
        return instance;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }
}
