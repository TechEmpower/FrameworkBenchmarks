plugins {
    application
    kotlin("jvm") version "2.0.21"
    kotlin("plugin.serialization") version "2.0.0"
    id("com.github.johnrengelman.shadow") version "8.1.0"
}

group = "org.jetbrains.ktor"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

application {
    mainClass = "io.ktor.server.netty.EngineMain"
}

val ktor_version = "3.1.2"
val vertx_version = "4.5.11"

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.8.1")
    implementation("io.ktor:ktor-server-netty:$ktor_version")
    implementation("io.ktor:ktor-server-html-builder-jvm:$ktor_version")
    implementation("io.ktor:ktor-server-default-headers-jvm:$ktor_version")
    implementation("io.vertx:vertx-pg-client:$vertx_version")
    implementation("io.vertx:vertx-lang-kotlin:$vertx_version")
    implementation("io.vertx:vertx-lang-kotlin-coroutines:$vertx_version")
    implementation("ch.qos.logback:logback-classic:1.5.12")
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(21)
    }
}

tasks.shadowJar {
    archiveBaseName.set("ktor-pgclient")
    archiveClassifier.set("")
    archiveVersion.set("")
}
