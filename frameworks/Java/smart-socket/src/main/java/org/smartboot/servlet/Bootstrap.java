package org.smartboot.servlet;

import org.smartboot.aio.EnhanceAsynchronousChannelProvider;
import org.smartboot.http.server.HttpMessageProcessor;
import org.smartboot.http.server.HttpRequestProtocol;
import org.smartboot.http.server.Request;
import org.smartboot.servlet.conf.ServletInfo;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.buffer.BufferFactory;
import org.smartboot.socket.buffer.BufferPagePool;
import org.smartboot.socket.extension.plugins.MonitorPlugin;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;
import org.smartboot.socket.transport.AioSession;

import java.io.IOException;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2020/12/22
 */
public class Bootstrap {

    public static void main(String[] args) {
        System.setProperty("java.nio.channels.spi.AsynchronousChannelProvider", EnhanceAsynchronousChannelProvider.class.getName());

        ServletHttpHandle httpHandle = new ServletHttpHandle();
        ContainerRuntime containerRuntime = new ContainerRuntime("/");
        // plaintext
        ServletInfo plainTextServletInfo = new ServletInfo();
        plainTextServletInfo.setServletName("plaintext");
        plainTextServletInfo.setServletClass(HelloWorldServlet.class.getName());
        plainTextServletInfo.addMapping("/plaintext");
        containerRuntime.getDeploymentInfo().addServlet(plainTextServletInfo);

        // json
        ServletInfo jsonServletInfo = new ServletInfo();
        jsonServletInfo.setServletName("json");
        jsonServletInfo.setServletClass(HelloWorldServlet.class.getName());
        jsonServletInfo.addMapping("/json");
        containerRuntime.getDeploymentInfo().addServlet(jsonServletInfo);
        httpHandle.addRuntime(containerRuntime);

        httpHandle.start();
        HttpMessageProcessor processor = new HttpMessageProcessor();
        processor.pipeline(httpHandle);
        http(processor);
    }

    public static void http(final HttpMessageProcessor processor) {
        AbstractMessageProcessor<Request> messageProcessor = new AbstractMessageProcessor<Request>() {
            @Override
            public void process0(AioSession session, Request msg) {
                processor.process(session, msg);
            }

            @Override
            public void stateEvent0(AioSession session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                processor.stateEvent(session, stateMachineEnum, throwable);
            }
        };
        messageProcessor.addPlugin(new MonitorPlugin(5));
//        messageProcessor.addPlugin(new SocketOptionPlugin());

        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        AioQuickServer<Request> server = new AioQuickServer<>(8080, new HttpRequestProtocol(), messageProcessor);
        server.setThreadNum(cpuNum + 2)
                .setReadBufferSize(1024 * 4)
                .setBufferFactory(new BufferFactory() {
                    @Override
                    public BufferPagePool create() {
                        return new BufferPagePool(10 * 1024 * 1024, cpuNum + 2, 64 * 1024 * 1024, true);
                    }
                })
                .setBannerEnabled(false)
                .setWriteBuffer(1024 * 4, 8);

//        messageProcessor.addPlugin(new BufferPageMonitorPlugin(server, 6));
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
