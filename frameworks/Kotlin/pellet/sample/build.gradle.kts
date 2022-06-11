plugins {
    application
    id("com.github.johnrengelman.shadow") version "7.1.0"
    kotlin("jvm") version "1.6.21"
    kotlin("plugin.serialization") version "1.6.21"
}

group = "benchmark"
version = "1.0.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("dev.pellet:pellet-server:0.0.7")
    implementation("dev.pellet:pellet-logging:0.0.7")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2")
    implementation(platform(kotlin("bom")))
    implementation(kotlin("stdlib-jdk8"))
    implementation(platform("org.jetbrains.kotlinx:kotlinx-coroutines-bom:1.6.1"))
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-jdk8")
}

java {
    toolchain {
        sourceCompatibility = JavaVersion.VERSION_18
        targetCompatibility = JavaVersion.VERSION_18
    }
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions.jvmTarget = "18"
}

application {
    mainClass.set("benchmark.BenchmarkKt")
}
