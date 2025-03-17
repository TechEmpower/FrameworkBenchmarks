plugins {
    application
    id("com.github.johnrengelman.shadow") version "7.1.0"
    kotlin("jvm") version "1.9.23"
    kotlin("plugin.serialization") version "1.9.23"
    id("nu.studer.rocker") version "3.0.4"
}

group = "benchmark"
version = "1.0.0"

repositories {
    mavenCentral()
}

rocker {
    version.set("1.3.0")
    configurations {
        create("main") {
            optimize.set(true)
            templateDir.set(file("src/main/resources"))
            outputDir.set(file("src/generated/rocker"))
        }
    }
}

dependencies {
    implementation(platform("dev.pellet:pellet-bom:0.0.16"))
    implementation("dev.pellet:pellet-server")
    implementation("dev.pellet:pellet-logging")
    implementation("org.slf4j:slf4j-api:1.7.36")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.3")
    implementation(platform(kotlin("bom")))
    implementation(kotlin("stdlib-jdk8"))
    implementation("io.vertx:vertx-pg-client:4.5.5")
    implementation("io.vertx:vertx-lang-kotlin:4.5.5")
    implementation("com.ongres.scram:client:2.1")
}

java {
    toolchain {
        sourceCompatibility = JavaVersion.VERSION_21
        targetCompatibility = JavaVersion.VERSION_21
    }
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
    kotlinOptions.jvmTarget = "21"
}

application {
    mainClass.set("benchmark.BenchmarkKt")
}
