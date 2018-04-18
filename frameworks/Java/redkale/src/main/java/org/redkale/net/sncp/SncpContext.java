/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.sncp;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.logging.Logger;
import javax.net.ssl.SSLContext;
import org.redkale.net.*;
import org.redkale.util.*;

/**
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class SncpContext extends Context {

    public SncpContext(long serverStartTime, Logger logger, ThreadPoolExecutor executor, SSLContext sslContext,
        int bufferCapacity, ObjectPool<ByteBuffer> bufferPool, ObjectPool<Response> responsePool,
        int maxbody, Charset charset, InetSocketAddress address, ResourceFactory resourceFactory,
        PrepareServlet prepare, int aliveTimeoutSeconds, int readTimeoutSeconds, int writeTimeoutSeconds) {
        super(serverStartTime, logger, executor, sslContext, bufferCapacity, bufferPool, responsePool,
            maxbody, charset, address, resourceFactory, prepare, aliveTimeoutSeconds, readTimeoutSeconds, writeTimeoutSeconds);
    }
}
