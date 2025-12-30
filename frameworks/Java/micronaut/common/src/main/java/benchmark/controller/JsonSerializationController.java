package benchmark.controller;

import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

import java.util.HashMap;
import java.util.Map;

@Controller("/json")
public class JsonSerializationController {

    // https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#json-serialization
    @Get
    public Map<String, String> getJson() {
        final Map<String, String> map = new HashMap<>();
        map.put("message", "Hello, World!");
        return map;
    }

}
