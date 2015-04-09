package benchmark;

import benchmark.entities.HelloWorld;
import com.kolich.curacao.annotations.Controller;
import com.kolich.curacao.annotations.RequestMapping;
import com.kolich.curacao.mappers.request.matchers.AntPathMatcher;

@Controller
public final class Benchmarks {

    @RequestMapping(value="/json", matcher=AntPathMatcher.class)
    public final HelloWorld json() {
        return new HelloWorld("Hello, World!");
    }

    @RequestMapping(value="/plaintext", matcher=AntPathMatcher.class)
    public final String plainText() {
        return "Hello, World!";
    }

}
