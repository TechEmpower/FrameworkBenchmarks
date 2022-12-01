package benchmark;

import io.micronaut.runtime.Micronaut;

public class Application {

    public static void main(String[] args) {
        Micronaut.build(args).environments("common").classes(Application.class).start();
    }

}