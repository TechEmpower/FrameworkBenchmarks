package benchmark;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.reactive.result.view.Rendering;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import static java.util.Comparator.comparing;

@Controller
public final class BenchmarkController {

    @GetMapping(value = "/plaintext", produces = "text/plain")
    @ResponseBody
    public Mono<String> plaintext() {
        return Mono.just("Hello, World!");
    }

    @GetMapping(value = "/json", produces = "application/json")
    @ResponseBody
    public Mono<Map<String, String>> json() {
        return Mono.just(Map.of("message", "Hello, World!"));
    }
}
