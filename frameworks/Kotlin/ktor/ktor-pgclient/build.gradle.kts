import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    application
    kotlin("jvm") version "2.1.21"
    kotlin("plugin.serialization") version "2.1.21"
    id("com.gradleup.shadow") version "8.3.9"
}

group = "org.jetbrains.ktor"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

application {
    mainClass = "io.ktor.server.netty.EngineMain"
}

val ktor_version = "3.3.3"
val vertx_version = "5.0.5"

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

kotlin {
    jvmToolchain(21)
}

tasks.withType<KotlinCompile>().configureEach {
    compilerOptions {
        jvmTarget.set(JvmTarget.JVM_21)
    }
}

tasks.shadowJar {
    archiveBaseName.set("ktor-pgclient")
    archiveClassifier.set("")
    archiveVersion.set("")
}
