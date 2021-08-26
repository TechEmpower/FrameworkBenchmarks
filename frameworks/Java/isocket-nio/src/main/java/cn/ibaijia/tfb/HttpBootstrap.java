package cn.ibaijia.tfb;

import cn.ibaijia.isocket.Server;
import cn.ibaijia.isocket.listener.SessionProcessErrorListener;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.processor.PlanTextProcessor;
import cn.ibaijia.tfb.protocol.SimpleHttpProtocol;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HttpBootstrap {
    private static final Logger logger = LoggerFactory.getLogger(HttpBootstrap.class);

    public static void main(String[] args) {
        Server server = new Server("0.0.0.0", 8080);
        server.addProtocol(new SimpleHttpProtocol());
        server.setProcessor(new PlanTextProcessor());
        server.setSessionProcessErrorListener(new SessionProcessErrorListener() {
            @Override
            public void run(Session session, Object o, Throwable throwable) {
                logger.error("session on process error.", throwable);
            }
        });
        int processorNumber = Runtime.getRuntime().availableProcessors();
        server.setThreadNumber(processorNumber);
//        server.setUseDirectBuffer(true);
        server.setPoolSize(16 * 1024);
        server.setReadBuffSize(4 * 1024);
        server.setBacklog(1024 * 8);
        server.start();
    }

}
