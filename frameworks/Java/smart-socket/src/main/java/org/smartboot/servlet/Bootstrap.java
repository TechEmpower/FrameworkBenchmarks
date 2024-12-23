package org.smartboot.servlet;


import tech.smartboot.servlet.Container;
import tech.smartboot.servlet.ServletContextRuntime;
import tech.smartboot.servlet.conf.ServletInfo;
import tech.smartboot.servlet.conf.ServletMappingInfo;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2020/12/22
 */
public class Bootstrap {

    public static void main(String[] args) throws Throwable {
        System.setProperty("smart-servlet-spring-boot-starter","true");
        Container containerRuntime = new Container();
        // plaintext
        ServletContextRuntime applicationRuntime = new ServletContextRuntime(null, Thread.currentThread().getContextClassLoader(), "/");
        applicationRuntime.setVendorProvider(response -> {

        });
        ServletInfo plainTextServletInfo = new ServletInfo();
        plainTextServletInfo.setServletName("plaintext");
        plainTextServletInfo.setServletClass(HelloWorldServlet.class.getName());
        applicationRuntime.getDeploymentInfo().addServletMapping(new ServletMappingInfo(plainTextServletInfo.getServletName(), "/plaintext"));
        applicationRuntime.getDeploymentInfo().addServlet(plainTextServletInfo);

        // json
        ServletInfo jsonServletInfo = new ServletInfo();
        jsonServletInfo.setServletName("json");
        jsonServletInfo.setServletClass(JsonServlet.class.getName());
        applicationRuntime.getDeploymentInfo().addServlet(jsonServletInfo);
        applicationRuntime.getDeploymentInfo().addServletMapping(new ServletMappingInfo(jsonServletInfo.getServletName(), "/json"));
        containerRuntime.addRuntime(applicationRuntime);
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        containerRuntime.getConfiguration()
                .setThreadNum(cpuNum)
                .setReadBufferSize(1024 * 4);
        containerRuntime.initialize();
        containerRuntime.start();
        
    }
}
