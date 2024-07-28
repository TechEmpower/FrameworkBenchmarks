plugins {
    application
    kotlin("jvm") version "1.9.22"
    kotlin("plugin.serialization") version "2.0.0"
    id("com.github.johnrengelman.shadow") version "8.1.0"
}

repositories {
    mavenCentral()
}

val ktorVersion = "2.3.12"
val kotlinxSerializationVersion = "1.6.3"
val exposedVersion = "0.52.0"

dependencies {
    implementation("io.ktor:ktor-server-core:$ktorVersion")
    implementation("io.ktor:ktor-server-netty:$ktorVersion")
    implementation("io.ktor:ktor-server-html-builder-jvm:$ktorVersion")
    implementation("io.ktor:ktor-server-default-headers-jvm:$ktorVersion")

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$kotlinxSerializationVersion")

    implementation("org.jetbrains.exposed:exposed-core:$exposedVersion")
    implementation("org.jetbrains.exposed:exposed-dao:$exposedVersion")
    implementation("org.jetbrains.exposed:exposed-jdbc:$exposedVersion")

    implementation("org.postgresql:postgresql:42.5.4")
    implementation("com.zaxxer:HikariCP:5.0.1")
    runtimeOnly("org.slf4j:slf4j-simple:1.7.36")
}

application.mainClass.set("AppKt")
