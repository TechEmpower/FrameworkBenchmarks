tasks.wrapper {
    distributionType = Wrapper.DistributionType.ALL
}

plugins {
    val kotlinVersion = "2.3.0-RC2"
    kotlin("jvm") version kotlinVersion
    kotlin("plugin.serialization") version kotlinVersion
    application
}

repositories {
    mavenCentral()
}

val vertxVersion = "5.0.5"
val kotlinxSerializationVersion = "1.9.0"
dependencies {
    implementation(platform("io.vertx:vertx-stack-depchain:$vertxVersion"))
    implementation("io.vertx:vertx-web")
    implementation("io.vertx:vertx-pg-client")
    //implementation("io.netty", "netty-transport-native-epoll", classifier = "linux-x86_64")
    implementation("io.netty", "netty-transport-native-io_uring", classifier = "linux-x86_64")
    implementation("io.vertx:vertx-lang-kotlin")
    implementation("io.vertx:vertx-lang-kotlin-coroutines")

    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.10.2")

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$kotlinxSerializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json-io:$kotlinxSerializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-io-core:0.8.2")

    implementation("org.jetbrains.kotlinx:kotlinx-html:0.12.0")
    implementation("org.jetbrains.kotlinx:kotlinx-datetime:0.7.1")
}

kotlin.jvmToolchain(25) // Kotlin doesn't support Java 25 yet

application.mainClass.set("MainKt")
