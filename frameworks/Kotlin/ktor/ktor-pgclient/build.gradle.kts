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

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.3")
    implementation("io.ktor:ktor-server-netty:$ktor_version")
    implementation("io.ktor:ktor-server-html-builder-jvm:$ktor_version")
    implementation("io.ktor:ktor-server-default-headers-jvm:$ktor_version")
    implementation("io.vertx:vertx-pg-client:4.5.8")
    implementation("io.vertx:vertx-lang-kotlin:4.5.8")
    implementation("io.vertx:vertx-lang-kotlin-coroutines:4.5.8")
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
    kotlinOptions.jvmTarget = "17"
}

tasks.shadowJar {
    archiveBaseName.set("ktor-pgclient")
    archiveClassifier.set("")
    archiveVersion.set("")
}
