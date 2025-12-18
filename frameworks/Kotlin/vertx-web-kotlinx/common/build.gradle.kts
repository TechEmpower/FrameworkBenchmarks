plugins {
    id("buildlogic.kotlin-library-conventions")
    alias(libs.plugins.kotlin.plugin.serialization)
}

dependencies {
    // TODO consider moving the GAVs to the version catalog too
    api(platform("io.vertx:vertx-stack-depchain:${libs.versions.vertx.get()}"))
    api("io.vertx:vertx-web")
    //runtimeOnly("io.netty", "netty-transport-native-epoll", classifier = "linux-x86_64")
    runtimeOnly("io.netty", "netty-transport-native-io_uring", classifier = "linux-x86_64")
    api("io.vertx:vertx-lang-kotlin")
    api("io.vertx:vertx-lang-kotlin-coroutines")

    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:${libs.versions.kotlinx.coroutines.get()}")

    api("org.jetbrains.kotlinx:kotlinx-serialization-json:${libs.versions.kotlinx.serialization.get()}")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json-io:${libs.versions.kotlinx.serialization.get()}")
    implementation("org.jetbrains.kotlinx:kotlinx-io-core:${libs.versions.kotlinx.io.get()}")

    implementation("org.jetbrains.kotlinx:kotlinx-datetime:${libs.versions.kotlinx.datetime.get()}")
}
