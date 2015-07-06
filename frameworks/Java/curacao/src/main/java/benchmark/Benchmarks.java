package benchmark;

import benchmark.entities.HelloWorld;
import curacao.annotations.Controller;
import curacao.annotations.RequestMapping;

@Controller
public final class Benchmarks {

    @RequestMapping("^\\/json$")
    public final HelloWorld json() {
        return new HelloWorld("Hello, World!");
    }

    @RequestMapping("^\\/plaintext$")
    public final String plainText() {
        return "Hello, World!";
    }

}
