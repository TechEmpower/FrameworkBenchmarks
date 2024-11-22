plugins {
    application
    kotlin("jvm") version "1.9.22"
    kotlin("plugin.serialization") version "2.0.0"
    id("com.github.johnrengelman.shadow") version "8.1.0"
}

group = "org.jetbrains.ktor"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

application {
    mainClass.set("MainKt")
}

val ktor_version = "2.3.12"
val kotlinx_serialization_version = "1.6.3"
val vertx_pg_client = "4.5.8"

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$kotlinx_serialization_version")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.8.1")
    implementation("io.ktor:ktor-server-netty:$ktor_version")
    implementation("io.ktor:ktor-server-default-headers:$ktor_version")
    implementation("io.ktor:ktor-server-html-builder:$ktor_version")
    implementation("com.github.jasync-sql:jasync-postgresql:2.2.0")
}

tasks.shadowJar {
    archiveBaseName.set("bench")
    archiveClassifier.set("")
    archiveVersion.set("")
}
