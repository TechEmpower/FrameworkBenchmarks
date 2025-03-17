package org.smartboot.servlet;


import tech.smartboot.servlet.Container;
import tech.smartboot.servlet.ServletContextRuntime;
import tech.smartboot.servlet.conf.ServletInfo;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2020/12/22
 */
public class Bootstrap {

    public static void main(String[] args) throws Throwable {
        System.setProperty("smart-servlet-spring-boot-starter", "true");
        Container containerRuntime = new Container();
        // plaintext
        ServletContextRuntime applicationRuntime = new ServletContextRuntime(null, Thread.currentThread().getContextClassLoader(), "/");
        applicationRuntime.setVendorProvider(response -> {

        });
        ServletInfo plainTextServletInfo = new ServletInfo();
        plainTextServletInfo.setServletName("plaintext");
        plainTextServletInfo.setServletClass(HelloWorldServlet.class.getName());
        plainTextServletInfo.addServletMapping("/plaintext", applicationRuntime);
        applicationRuntime.getDeploymentInfo().addServlet(plainTextServletInfo);

        // json
        ServletInfo jsonServletInfo = new ServletInfo();
        jsonServletInfo.setServletName("json");
        jsonServletInfo.setServletClass(JsonServlet.class.getName());
        jsonServletInfo.addServletMapping("/json", applicationRuntime);
        applicationRuntime.getDeploymentInfo().addServlet(jsonServletInfo);
        containerRuntime.addRuntime(applicationRuntime);
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        containerRuntime.getConfiguration()
                .setThreadNum(cpuNum)
                .setHeaderLimiter(0)
                .setReadBufferSize(1024 * 4);
        containerRuntime.initialize();
        containerRuntime.start();

    }
}
