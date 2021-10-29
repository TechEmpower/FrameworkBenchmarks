module io.vertx.core {
    requires io.netty.handler.proxy;
    requires io.netty.resolver.dns;
    requires io.netty.transport.epoll;
    requires io.netty.transport.unix.common;
    requires java.naming;
    requires org.apache.logging.log4j;

    requires transitive com.fasterxml.jackson.core;
    requires transitive com.fasterxml.jackson.databind;
    requires transitive io.netty.buffer;
    requires transitive io.netty.codec;
    requires transitive io.netty.codec.dns;
    requires transitive io.netty.codec.http;
    requires transitive io.netty.codec.http2;
    requires transitive io.netty.common;
    requires transitive io.netty.handler;
    requires transitive io.netty.resolver;
    requires transitive io.netty.transport;
    requires transitive java.compiler;
    requires transitive java.logging;

    exports io.vertx.core;
    exports io.vertx.core.buffer;
    exports io.vertx.core.buffer.impl;
    exports io.vertx.core.cli;
    exports io.vertx.core.cli.annotations;
    exports io.vertx.core.cli.converters;
    exports io.vertx.core.cli.impl;
    exports io.vertx.core.datagram;
    exports io.vertx.core.datagram.impl;
    exports io.vertx.core.dns;
    exports io.vertx.core.dns.impl;
    exports io.vertx.core.dns.impl.decoder;
    exports io.vertx.core.eventbus;
    exports io.vertx.core.eventbus.impl;
    exports io.vertx.core.eventbus.impl.clustered;
    exports io.vertx.core.eventbus.impl.codecs;
    exports io.vertx.core.file;
    exports io.vertx.core.file.impl;
    exports io.vertx.core.http;
    exports io.vertx.core.http.impl;
    exports io.vertx.core.http.impl.cgbystrom;
    exports io.vertx.core.http.impl.headers;
    exports io.vertx.core.http.impl.ws;
    exports io.vertx.core.impl;
    exports io.vertx.core.impl.cpu;
    exports io.vertx.core.impl.future;
    exports io.vertx.core.impl.launcher;
    exports io.vertx.core.impl.launcher.commands;
    exports io.vertx.core.impl.logging;
    exports io.vertx.core.impl.resolver;
    exports io.vertx.core.impl.utils;
    exports io.vertx.core.impl.verticle;
    exports io.vertx.core.json;
    exports io.vertx.core.json.impl;
    exports io.vertx.core.json.jackson;
    exports io.vertx.core.json.pointer;
    exports io.vertx.core.json.pointer.impl;
    exports io.vertx.core.logging;
    exports io.vertx.core.metrics;
    exports io.vertx.core.metrics.impl;
    exports io.vertx.core.net;
    exports io.vertx.core.net.impl;
    exports io.vertx.core.net.impl.pkcs1;
    exports io.vertx.core.net.impl.pool;
    exports io.vertx.core.net.impl.transport;
    exports io.vertx.core.parsetools;
    exports io.vertx.core.parsetools.impl;
    exports io.vertx.core.shareddata;
    exports io.vertx.core.shareddata.impl;
    exports io.vertx.core.spi;
    exports io.vertx.core.spi.cluster;
    exports io.vertx.core.spi.cluster.impl;
    exports io.vertx.core.spi.cluster.impl.selector;
    exports io.vertx.core.spi.json;
    exports io.vertx.core.spi.launcher;
    exports io.vertx.core.spi.logging;
    exports io.vertx.core.spi.metrics;
    exports io.vertx.core.spi.observability;
    exports io.vertx.core.spi.resolver;
    exports io.vertx.core.spi.tracing;
    exports io.vertx.core.streams;
    exports io.vertx.core.streams.impl;
    exports io.vertx.core.tracing;

    provides io.vertx.core.spi.launcher.CommandFactory with
        io.vertx.core.impl.launcher.commands.RunCommandFactory,
        io.vertx.core.impl.launcher.commands.VersionCommandFactory,
        io.vertx.core.impl.launcher.commands.BareCommandFactory,
        io.vertx.core.impl.launcher.commands.ListCommandFactory,
        io.vertx.core.impl.launcher.commands.StartCommandFactory,
        io.vertx.core.impl.launcher.commands.StopCommandFactory;
    
    uses io.vertx.core.spi.VertxServiceProvider;
    uses io.vertx.core.spi.VerticleFactory;
    uses io.vertx.core.spi.JsonFactory;

}
