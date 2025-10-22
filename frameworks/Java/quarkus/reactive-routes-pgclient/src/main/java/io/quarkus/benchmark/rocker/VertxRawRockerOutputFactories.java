package io.quarkus.benchmark.rocker;

import com.fizzed.rocker.RockerOutputFactory;
import io.netty.util.concurrent.FastThreadLocal;
import io.vertx.core.Context;
import jakarta.inject.Singleton;

@Singleton
public class VertxRawRockerOutputFactories {

    private final FastThreadLocal<RockerOutputFactory<VertxRawRockerOutput>> ioPool;

    VertxRawRockerOutputFactories() {
        ioPool = new FastThreadLocal<>() {
            @Override
            protected RockerOutputFactory<VertxRawRockerOutput> initialValue() {
                if (!Context.isOnEventLoopThread()) {
                    return null;
                }
                return VertxRawRockerOutput.factory();
            }
        };
    }

    public RockerOutputFactory<VertxRawRockerOutput> ioFactory() {
        return ioPool.get();
    }

}
