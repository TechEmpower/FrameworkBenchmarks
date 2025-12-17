import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    application
    kotlin("jvm") version "2.3.0-RC3"
    kotlin("plugin.serialization") version "2.3.0-RC3"
    id("com.gradleup.shadow") version "9.3.0"
}

repositories {
    mavenCentral()
}

val ktorVersion = "3.3.3"
val kotlinxSerializationVersion = "1.9.0"
val exposedVersion = "1.0.0-rc-4"

dependencies {
    implementation("io.ktor:ktor-server-core:$ktorVersion")
    implementation("io.ktor:ktor-server-netty:$ktorVersion")
    implementation("io.ktor:ktor-server-html-builder-jvm:$ktorVersion")
    implementation("io.ktor:ktor-server-default-headers-jvm:$ktorVersion")

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$kotlinxSerializationVersion")

    implementation("org.jetbrains.exposed:exposed-core:$exposedVersion")
    implementation("org.jetbrains.exposed:exposed-dao:$exposedVersion")
    implementation("org.jetbrains.exposed:exposed-jdbc:$exposedVersion")
    implementation("org.jetbrains.exposed:exposed-r2dbc:${exposedVersion}")

    implementation("org.postgresql:postgresql:42.7.8")
    implementation("com.zaxxer:HikariCP:7.0.2")

    implementation("org.postgresql:r2dbc-postgresql:1.1.1.RELEASE")
    implementation("io.r2dbc:r2dbc-pool:1.0.2.RELEASE")

    runtimeOnly("org.slf4j:slf4j-simple:2.0.7")
}

application.mainClass.set("AppKt")

kotlin. jvmToolchain(25)
