package com.example.starter.dagger

import com.example.starter.db.FortuneRepository
import com.example.starter.db.WorldRepository
import dagger.Module
import dagger.Provides
import io.vertx.pgclient.PgConnection
import jakarta.inject.Singleton

@Module
class RepositoryModule {
    @Provides
    @Singleton
    fun provideFortuneRepository(conn: PgConnection): FortuneRepository = FortuneRepository(conn)

    @Provides
    @Singleton
    fun provideWorldRepository(conn: PgConnection): WorldRepository = WorldRepository(conn)
}