module r2dbc.postgresql {
    requires com.ongres.scram.client;
    requires com.ongres.scram.common;
    requires io.netty.codec;
    requires io.netty.resolver;
    requires io.netty.transport;
    requires io.netty.transport.epoll;
    requires io.netty.transport.unix.common;
    requires java.naming;

    requires transitive io.netty.buffer;
    requires transitive io.netty.common;
    requires transitive io.netty.handler;
    requires transitive org.reactivestreams;
    requires transitive r2dbc.spi;
    requires transitive reactor.core;
    requires transitive reactor.netty.core;

    exports io.r2dbc.postgresql;
    exports io.r2dbc.postgresql.api;
    exports io.r2dbc.postgresql.authentication;
    exports io.r2dbc.postgresql.client;
    exports io.r2dbc.postgresql.codec;
    exports io.r2dbc.postgresql.extension;
    exports io.r2dbc.postgresql.message;
    exports io.r2dbc.postgresql.message.backend;
    exports io.r2dbc.postgresql.message.frontend;
    exports io.r2dbc.postgresql.replication;
    exports io.r2dbc.postgresql.util;

    provides io.r2dbc.postgresql.extension.Extension with
        io.r2dbc.postgresql.codec.BuiltinDynamicCodecs;
    provides io.r2dbc.spi.ConnectionFactoryProvider with
        io.r2dbc.postgresql.PostgresqlConnectionFactoryProvider;
    
    uses io.r2dbc.postgresql.extension.Extension;

}
