package cn.ibaijia.tfb;

import cn.ibaijia.isocket.Server;
import cn.ibaijia.isocket.listener.SessionProcessErrorListener;
import cn.ibaijia.isocket.protocol.SimpleHttpProtocol;
import cn.ibaijia.isocket.protocol.http.DateUtil;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.processor.PlanTextProcessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author longzl
 */
public class HttpBootstrap {
    private static final Logger logger = LoggerFactory.getLogger(HttpBootstrap.class);

    public static void main(String[] args) {
        Server server = new Server("0.0.0.0", 8080);
        server.addProtocol(new SimpleHttpProtocol());
        server.setProcessor(new PlanTextProcessor());
        server.setSessionProcessErrorListener((session, o, throwable) -> logger.error("session on process error.", throwable));
        server.setUseDirectBuffer(true);
        server.setUsePool(true);
        server.setPoolSize(1 * 1024);
        server.setBuffSize(1 * 1024);
        server.setBacklog(4 * 1024);
        server.start();
    }

}
