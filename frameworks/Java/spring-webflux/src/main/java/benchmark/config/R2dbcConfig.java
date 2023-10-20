package benchmark.config;

import org.springframework.boot.autoconfigure.r2dbc.R2dbcAutoConfiguration;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("r2dbc")
// Explicitly import the configuration as it needs to be disabled globally to
// support multiple persistence technologies on the classpath but only one selected
// for execution
@Import(R2dbcAutoConfiguration.class)
public class R2dbcConfig {}
