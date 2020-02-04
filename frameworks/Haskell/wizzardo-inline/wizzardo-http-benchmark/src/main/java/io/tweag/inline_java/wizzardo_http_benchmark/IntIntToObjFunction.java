package io.tweag.inline_java.wizzardo_http_benchmark;

@FunctionalInterface
public interface IntIntToObjFunction<T> {
    T apply(int a, int b);
}
