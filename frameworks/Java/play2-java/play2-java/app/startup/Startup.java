package startup;

import com.typesafe.config.Config;
import play.Logger;

import jakarta.inject.Inject;
import jakarta.inject.Singleton;

@Singleton
public class Startup {

    private static final Logger.ALogger logger = Logger.of(Startup.class);

    @Inject
    public Startup(final Config config) {
        logger.info("System properties");
        logger.info("-----------------");
        logger.info("physical_cpu_count: {}", System.getProperty("physical_cpu_count"));
        logger.info("thread_count: {}", System.getProperty("thread_count"));
        logger.info("");
        logger.info("Configuration");
        logger.info("-------------");
        logger.info("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max: {}", config.getInt("pekko.actor.default-dispatcher.fork-join-executor.parallelism-max"));
        logger.info("");
    }
}
