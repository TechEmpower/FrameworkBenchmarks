package io.quarkus.benchmark.rocker;

import com.fizzed.rocker.RockerOutputFactory;
import io.vertx.core.buffer.Buffer;

public interface VertxRawRockerOutput extends com.fizzed.rocker.RockerOutput<VertxRawRockerOutput> {

    // factory
    static RockerOutputFactory<VertxRawRockerOutput> factory() {
        return RawRockerOutput.raw();
    }

    Buffer buffer();

}
