package benchmark;

import benchmark.entities.HelloWorld;
import curacao.annotations.Controller;
import curacao.annotations.RequestMapping;
import curacao.mappers.request.matchers.CuracaoAntPathMatcher;

@Controller
public final class Benchmarks {

    @RequestMapping(value="/json", matcher=CuracaoAntPathMatcher.class)
    public final HelloWorld json() {
        return new HelloWorld("Hello, World!");
    }

    @RequestMapping(value="/plaintext", matcher=CuracaoAntPathMatcher.class)
    public final String plainText() {
        return "Hello, World!";
    }

}
