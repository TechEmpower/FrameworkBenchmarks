/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import org.smartboot.socket.transport.AioQuickServer;

import java.io.IOException;

public class HttpBootstrap {

    public static void main(String[] args) {
        AioQuickServer<HttpEntityV2> server = new AioQuickServer<HttpEntityV2>(8080, new HttpServerV2Protocol(), new HttpV2MessageProcessor());
        server.setWriteQueueSize(0)
                .setReadBufferSize(1280)
        ;

        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
