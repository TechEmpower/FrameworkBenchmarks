package com.example.starter.dagger

import com.example.starter.db.FortuneRepository
import com.example.starter.db.WorldRepository
import com.example.starter.handlers.DefaultHandler
import com.example.starter.handlers.FortuneHandler
import com.example.starter.handlers.MessageHandler
import com.example.starter.handlers.WorldHandler
import dagger.Module
import dagger.Provides
import jakarta.inject.Singleton

@Module
class HandlerModule {
    @Provides
    @Singleton
    fun provideDefaultHandler(): DefaultHandler = DefaultHandler()

    @Provides
    @Singleton
    fun provideFortuneHandler(repository: FortuneRepository): FortuneHandler = FortuneHandler(repository)

    @Provides
    @Singleton
    fun provideMessageHandler(): MessageHandler = MessageHandler()

    @Provides
    @Singleton
    fun provideWorldHandler(repository: WorldRepository): WorldHandler = WorldHandler(repository)
}