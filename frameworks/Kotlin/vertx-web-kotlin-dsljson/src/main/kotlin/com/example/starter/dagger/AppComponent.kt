package com.example.starter.dagger

import dagger.BindsInstance
import dagger.Component
import io.vertx.core.Vertx
import io.vertx.core.http.HttpServer
import io.vertx.pgclient.PgConnection
import jakarta.inject.Singleton

@Singleton
@Component(
    modules = [
        RepositoryModule::class,
        HandlerModule::class,
        ServerModule::class,
    ]
)
interface AppComponent {

    fun httpServer(): HttpServer

    @Component.Builder
    interface Builder {
        @BindsInstance
        fun vertx(vertx: Vertx): Builder

        @BindsInstance
        fun pgConnection(conn: PgConnection): Builder

        fun build(): AppComponent
    }
}
