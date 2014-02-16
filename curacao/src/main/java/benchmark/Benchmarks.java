package benchmark;

import benchmark.entities.HelloWorld;
import com.kolich.curacao.annotations.Controller;
import com.kolich.curacao.annotations.methods.GET;

@Controller
public final class Benchmarks {

    @GET("/json")
    public final HelloWorld json() {
        return new HelloWorld("Hello, World!");
    }

    @GET("/plaintext")
    public final String plainText() {
        return "Hello, World!";
    }

}
