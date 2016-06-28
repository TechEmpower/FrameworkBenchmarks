package com.techempower.beyondj;


import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import com.lenox.beyond.LaunchUtil;
import com.lenox.beyond.configuration.Config;
import com.lenox.beyondj.actor.ApplicationConfiguration;
import com.lenox.beyondj.actor.system.SystemSingletonService;
import com.lenox.beyondj.actor.system.model.AppLaunchCompletedMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;
import java.net.JarURLConnection;
import java.net.URL;
import java.security.CodeSource;
import java.security.ProtectionDomain;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author nickk
 */

public class BeyondJApplication {

    public static void main(String[] args) throws Exception {

        System.out.println("Start Launching Beyondj");

        if (args != null && args.length > 0) {
            for (String arg : args) {
                Map<String, String> map = convert(arg);
                Iterator<String> iterator = map.keySet().iterator();
                while (iterator.hasNext()) {
                    String key = iterator.next();
                    String value = map.get(key);
                    System.setProperty(key, value);
                    System.out.println("Set System Property " + key + " to " + value);
                }
            }
        }

        ApplicationContext context = new ClassPathXmlApplicationContext("/config/spring/core-spring-and-camel-config.xml");
        extractResources(context);

        BeyondJApplicationService service = (BeyondJApplicationService) context.getBean(ApplicationConfiguration.BEYOND_J_APPLICATION_SERVICE);

        ActorSystem system = service.launchActorSystem(context);
        int numLaunched = service.launchApplications(system);
        service.launchServices(context);

        if (numLaunched == 0) {
            SystemSingletonService systemSingletonService = (SystemSingletonService) context.getBean("systemSingletonService");
            AppLaunchCompletedMessage completedMessage = new AppLaunchCompletedMessage();
            ActorRef postLaunchActor = systemSingletonService.getPostSystemLaunchActor();
            postLaunchActor.tell(completedMessage, null);
        }

        LOG.info("BeyondJ System Started. Press CNTRL-C To Exit ");

        system.awaitTermination();
    }

    private static Map<String, String> convert(String str) {
        String[] tokens = str.split(" |=");
        Map<String, String> map = new HashMap<>();
        for (int i = 0; i < tokens.length - 1; ) map.put(tokens[i++], tokens[i++]);
        return map;
    }

    private static void extractResources(ApplicationContext context) throws Exception {

        Config config = (Config) context.getBean(ApplicationConfiguration.CONFIG);

        LaunchUtil.defaultConfig = config;

        LOG.debug("Resource install path is {}", config.getProperty(ApplicationConfiguration.BEYONDJ_HOME));
        System.out.println("Resource install path is " + config.getProperty(ApplicationConfiguration.BEYONDJ_HOME));

        String path = LaunchUtil.getContainerInstallationDir(config);
        ProtectionDomain protectionDomain = BeyondJApplication.class.getProtectionDomain();
        CodeSource codeSource = protectionDomain.getCodeSource();
        URL location = codeSource.getLocation();

        File installationDir = new File(path);

        if (codeSource.toString().indexOf(".jar") != -1) {
            if (codeSource.toString().indexOf(".jar!") != -1) {
                LOG.debug("Copying jar resources to install folder");
                System.out.println("Copying jar resources to install folder");
                JarURLConnection conn = (JarURLConnection) location.openConnection();
                LaunchUtil.copyJarResourcesToFolder(conn, installationDir);
            } else {
                //assume we launched this from the bin folder
                LOG.debug("Extracting jar resources to install folder");
                System.out.println("Extracting jar resources to install folder");
                File locFile = new File(location.toURI());
                LaunchUtil.extractJar(locFile.getAbsolutePath(), installationDir.getAbsolutePath());
            }
        } else {
            // throw new IllegalStateException("Not able to install platform. File format must be JAR");
        }

        String webAppsPath = path + File.separator + ApplicationConfiguration.LAUNCHERS + File.separator
                + ApplicationConfiguration.WEB_APP_LAUNCHERS;
        File webAppLaunchersDir = new File(webAppsPath);

        String[] fileNames = webAppLaunchersDir.list();
        if (fileNames != null) {
            LOG.debug("Extracting web-app resources to install folder");
            System.out.println("Extracting web-app resources to install folder");
            for (String fileName : fileNames) {
                String jarOrWarName = webAppsPath + File.separator + fileName.replace(".jar", "");
                jarOrWarName = jarOrWarName.replace(".war", "");
                LaunchUtil.extractJar(webAppsPath + File.separator + fileName, jarOrWarName);
                LaunchUtil.extractZip(webAppsPath + File.separator + fileName, webAppsPath + File.separator + fileName.replace(".zip", ""));
                LaunchUtil.extractTar(webAppsPath + File.separator + fileName);
            }
        }

        //----------------------------------------

        String jarAppsPath = path + File.separator + ApplicationConfiguration.LAUNCHERS + File.separator
                + ApplicationConfiguration.JAR_APP_LAUNCHERS;
        File jarAppLaunchersDir = new File(jarAppsPath);
        fileNames = jarAppLaunchersDir.list();

        if (fileNames != null) {
            LOG.debug("Extracting jar-app resources to install folder");
            System.out.println("Extracting jar-app resources to install folder");
            for (String fileName : fileNames) {
                String jarOrWarName = jarAppsPath + File.separator + fileName.replace(".jar", "");
                jarOrWarName = jarOrWarName.replace(".tar", "");
                LaunchUtil.extractJar(jarAppsPath + File.separator + fileName, jarOrWarName);
                LaunchUtil.extractZip(jarAppsPath + File.separator + fileName, jarAppsPath + File.separator + fileName.replace(".zip", ""));
                LaunchUtil.extractTar(jarAppsPath + File.separator + fileName);
            }
        }

        //----------------------------------------

        String scriptAppsPath = path + File.separator + ApplicationConfiguration.LAUNCHERS + File.separator
                + ApplicationConfiguration.SCRIPT_APP_LAUNCHERS;
        File scriptAppLaunchersDir = new File(scriptAppsPath);
        fileNames = scriptAppLaunchersDir.list();

        if (fileNames != null) {
            LOG.debug("Extracting script-app resources to install folder");
            System.out.println("Extracting script-app resources to install folder");
            for (String fileName : fileNames) {
                String jarOrWarName = scriptAppsPath + File.separator + fileName.replace(".jar", "");
                jarOrWarName = jarOrWarName.replace(".tar", "");
                LaunchUtil.extractJar(scriptAppsPath + File.separator + fileName, jarOrWarName);
                LaunchUtil.extractZip(scriptAppsPath + File.separator + fileName, scriptAppsPath + File.separator + fileName.replace(".zip", ""));
                LaunchUtil.extractTar(scriptAppsPath + File.separator + fileName);
            }
        }
    }

    private static final Logger LOG = LoggerFactory.getLogger(BeyondJApplication.class);
}
