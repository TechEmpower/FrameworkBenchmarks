package benchmark;

import io.micronaut.context.annotation.ConfigurationProperties;
import io.micronaut.context.annotation.Context;
import io.micronaut.context.annotation.Requires;
import io.micronaut.context.event.ApplicationEventListener;
import io.micronaut.runtime.event.ApplicationShutdownEvent;
import one.profiler.AsyncProfiler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.Socket;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

@Context
@Requires(property = "async-profiler.args")
public class AsyncProfilerHelper implements ApplicationEventListener<ApplicationShutdownEvent> {
    private static final Logger log = LoggerFactory.getLogger(AsyncProfilerHelper.class);

    private final AsyncProfilerHelper.AsyncProfilerConfiguration configuration;
    private final Path tmp;

    AsyncProfilerHelper(AsyncProfilerConfiguration configuration) throws Exception {
        this.configuration = configuration;
        tmp = Files.createTempFile("output", null);
        String rsp = AsyncProfiler.getInstance().execute(configuration.args + ",file=" + tmp);
        log.info("Started async-profiler: {}", rsp);
    }

    @Override
    public void onApplicationEvent(ApplicationShutdownEvent event) {
        try {
            stop();
        } catch (Exception e) {
            log.error("Failed to stop async-profiler", e);
        }
    }

    private void stop() throws Exception {
        AsyncProfiler.getInstance().execute("stop,jfr,file=" + tmp);
        log.info("Stopped async-profiler");
        URI uri = URI.create(configuration.dump);
        log.info("Sending async-profiler result to {} ({} bytes)", uri, Files.size(tmp));
        if (uri.getScheme().equals("tcp")) {
            try (Socket socket = new Socket(uri.getHost(), uri.getPort())) {
                Files.copy(tmp, socket.getOutputStream());
            }
        } else if (uri.getScheme().equals("file")) {
            Files.copy(tmp, Path.of(uri), StandardCopyOption.REPLACE_EXISTING);
        } else {
            throw new UnsupportedOperationException("Unsupported URI: " + uri);
        }
        log.info("Async-profiler result sent");
    }

    @ConfigurationProperties("async-profiler")
    record AsyncProfilerConfiguration(
            String args,
            String dump
    ) {
    }
}
