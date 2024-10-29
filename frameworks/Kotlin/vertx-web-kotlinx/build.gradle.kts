tasks.wrapper {
    distributionType = Wrapper.DistributionType.ALL
}

plugins {
    val kotlinVersion = "2.0.21"
    kotlin("jvm") version kotlinVersion
    kotlin("plugin.serialization") version kotlinVersion
    application
}

repositories {
    mavenCentral()
}

val vertxVersion = "4.5.10"
val kotlinxSerializationVersion = "1.7.3"
dependencies {
    implementation(platform("io.vertx:vertx-stack-depchain:$vertxVersion"))
    implementation("io.vertx:vertx-web")
    implementation("io.vertx:vertx-pg-client")
    implementation("io.netty", "netty-transport-native-epoll", classifier = "linux-x86_64")
    implementation("io.vertx:vertx-lang-kotlin")
    implementation("io.vertx:vertx-lang-kotlin-coroutines")
    runtimeOnly("io.vertx:vertx-io_uring-incubator")
    // This dependency has to be added for io_uring to work.
    runtimeOnly("io.netty.incubator:netty-incubator-transport-native-io_uring:0.0.25.Final:linux-x86_64")

    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.9.0")

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$kotlinxSerializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json-io:$kotlinxSerializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-io-core:0.5.4")

    implementation("org.jetbrains.kotlinx:kotlinx-html:0.11.0")
    //implementation("org.jetbrains.kotlinx:kotlinx-datetime:0.4.0") // the latest version is 0.6.1
}

kotlin.jvmToolchain(21)

application.mainClass.set("MainKt")
