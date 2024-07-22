import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
    kotlin("jvm") version "1.9.22"
    kotlin("kapt") version "1.9.22"
    application
    id("com.github.johnrengelman.shadow") version "7.1.2"
}

group = "com.example"
version = "1.0.0-SNAPSHOT"

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

kotlin {
    compilerOptions {
        jvmTarget = JvmTarget.JVM_21
    }
    jvmToolchain(21)
}

val mainClassName = "com.example.starter.App"

val vertxVersion = "4.5.4"
val nettyVersion = "4.1.111.Final"
val daggerVersion = "2.51"
val jacksonVersion = "2.17.2"
val dslJsonVersion = "2.0.2"
val htmlFlowVersion = "4.6"
val log4jVersion = "2.23.0"

application {
    mainClass = mainClassName
}

dependencies {
    listOf(
        // Kotlin
        kotlin("stdlib-jdk8"),
        kotlin("reflect"),

        // Vertx
        platform("io.vertx:vertx-stack-depchain:$vertxVersion"),
        "io.vertx:vertx-core",
        "io.vertx:vertx-web",
        "io.vertx:vertx-pg-client",
        "io.vertx:vertx-lang-kotlin",
        "io.vertx:vertx-lang-kotlin-coroutines",

        // Modules
        "jakarta.inject:jakarta.inject-api:2.0.1",
        "com.google.dagger:dagger:$daggerVersion",

        // Netty
        "io.netty:netty-transport-native-epoll:$nettyVersion:linux-x86_64",

        "com.ongres.scram:client:2.1",

        // Jackson
        "com.fasterxml.jackson.module:jackson-module-blackbird:$jacksonVersion",
        "com.fasterxml.jackson.datatype:jackson-datatype-jsr310:$jacksonVersion",
        "com.fasterxml.jackson.module:jackson-module-kotlin:$jacksonVersion",

        // dsljson
        "com.dslplatform:dsl-json:$dslJsonVersion",

        // HtmlFlow
        "com.github.xmlet:htmlflow:$htmlFlowVersion",

        // Logging
        "org.apache.logging.log4j:log4j-core:$log4jVersion",
        "org.apache.logging.log4j:log4j-api:$log4jVersion",
        "org.apache.logging.log4j:log4j-api-kotlin:1.4.0",
        "com.lmax:disruptor:4.0.0",
    ).map { implementation(it) }

    listOf(
        "com.google.dagger:dagger-compiler:$daggerVersion",
        "com.dslplatform:dsl-json:$dslJsonVersion",
        "org.apache.logging.log4j:log4j-core:$log4jVersion",
    ).map { kapt(it) }
}

tasks.withType<ShadowJar> {
    archiveClassifier = "fat"
    mergeServiceFiles()
}
