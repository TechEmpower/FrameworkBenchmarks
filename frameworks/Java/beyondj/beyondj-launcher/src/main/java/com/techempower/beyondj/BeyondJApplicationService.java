package com.techempower.beyondj;


import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import com.lenox.beyond.LaunchUtil;
import com.lenox.beyond.configuration.Config;
import com.lenox.beyondj.actor.ApplicationConfiguration;
import com.lenox.beyondj.extension.SpringNameExtension;
import com.lenox.beyondj.extension.SpringTypeExtension;
import com.lenox.beyondj.ha.install.HazelcastInstallationDelegate;
import com.lenox.beyondj.services.ApplicationLaunchService;
import com.lenox.beyondj.services.BeyondJManagementService;
import com.lenox.common.sigarprocess.ProcessManager;
import com.lenox.common.sigarprocess.ProcessManagerFactory;
import com.lenox.configuration.ApplicationContextProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

import java.io.File;
import java.security.CodeSource;
import java.security.ProtectionDomain;
import java.util.Arrays;

/**
 * @author nickk
 */

public class BeyondJApplicationService {

    private static ActorRef supervisor;
    private static SpringNameExtension springNameExtension;
    private static SpringTypeExtension springTypeExtension;
    private ActorSystem system;
    private static HazelcastInstallationDelegate installationDelegate;
    private static ApplicationLaunchService applicationLaunchService;

    public void launchServices(ApplicationContext context) {

        Config config = (Config) context.getBean(ProcessManagerFactory.CONFIG);
        ProcessManager processManager = null;

        try {
            processManager = ProcessManagerFactory.createProcessManager(config, BeyondJApplicationService.class);

            LOG.info("Platform Process Manager found is {} ", processManager.getClass());
        } catch (Throwable t) {
            LOG.error("Error creating process manager", t);
            throw new RuntimeException(t);
        }
        this.installationDelegate = (HazelcastInstallationDelegate) ApplicationContextProvider
                .getApplicationContext().getBean(ApplicationConfiguration.INSTALLATION_DELEGATE);
    }

    public ActorSystem launchActorSystem(ApplicationContext context) {

        ActorSystem system = context.getBean(ActorSystem.class);
        springNameExtension = context.getBean(SpringNameExtension.class);
        springTypeExtension = context.getBean(SpringTypeExtension.class);

        // Use the Spring Extension to create props for a named actor bean
        supervisor = system.actorOf(
                springNameExtension.props(ApplicationConfiguration.SUPERVISOR));

        LOG.info("Platform Actor System Started");
        return system;
    }

    public int launchApplications(ActorSystem system) throws Exception {

        if (this.system == null) {
            this.system = system;
        }

        this.applicationLaunchService = (ApplicationLaunchService) ApplicationContextProvider
                .getApplicationContext().getBean(ApplicationConfiguration.APPLICATION_LAUNCH_SERVICE);

        if (applicationLaunchService == null) {
            LOG.error("Null applicationLaunchService found");
            return 0;
        }

        this.installationDelegate = (HazelcastInstallationDelegate) ApplicationContextProvider
                .getApplicationContext().getBean(ApplicationConfiguration.INSTALLATION_DELEGATE);

        ProtectionDomain protectionDomain = BeyondJApplication.class.getProtectionDomain();
        CodeSource codeSource = protectionDomain.getCodeSource();

        String baseLocation = ApplicationLaunchService.baseLocation(BeyondJApplication.class);

        if (!baseLocation.contains("target/classes/")) {
            if (codeSource.toString().indexOf(".jar!") < 0) {
                baseLocation = baseLocation.substring(baseLocation.indexOf("/"), baseLocation.lastIndexOf("/")) + File.separator + "conf";
            }
        }

        String configDirStr = null;
        if (codeSource.toString().indexOf(".jar!") != -1) {
            Config config = (Config) ApplicationContextProvider.getApplicationContext().getBean(ApplicationConfiguration.CONFIG);
            String dir = LaunchUtil.getContainerInstallationDir(config);
            configDirStr = dir + File.separator + "launchers" + File.separator + "config";
        } else {
            configDirStr = baseLocation + File.separator + "launchers" + File.separator + "config";
        }

        File configDir = new File(configDirStr);
        String[] files = configDir.list();

        applicationLaunchService.setSpringNameExtension(springNameExtension);
        applicationLaunchService.setSpringTypeExtension(springTypeExtension);
        applicationLaunchService.setSupervisor(supervisor);
        applicationLaunchService.setSystem(system);

        if (files == null || files.length == 0) {
            LOG.error("No config files found in {}. Not launching any applications", configDirStr);
            return 0;
        }

        BeyondJManagementService managementService = (BeyondJManagementService) ApplicationContextProvider
                .getApplicationContext().getBean(ApplicationConfiguration.MANAGEMENT_SERVICE);

        managementService.setActorSystem(system);

        String clusterLocation = installationDelegate.getLocalEndpoint();
        return applicationLaunchService.launchApplications(BeyondJApplication.class, Arrays.asList(files), clusterLocation);
    }

    private static final Logger LOG = LoggerFactory.getLogger(BeyondJApplicationService.class);
}
