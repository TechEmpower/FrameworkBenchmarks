package benchmark;

import io.micronaut.runtime.Micronaut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Application {

    private static final Logger log = LoggerFactory.getLogger(Application.class);

    public static void main(String[] args) {
        log.info("Runtime.maxMemory: {}",  Runtime.getRuntime().maxMemory());
        log.info("Runtime.totalMemory: {}",  Runtime.getRuntime().totalMemory());
        log.info("Runtime.availableProcessors: {}",  Runtime.getRuntime().availableProcessors());

        Micronaut.build(args).environments("common").classes(Application.class).start();
    }

}