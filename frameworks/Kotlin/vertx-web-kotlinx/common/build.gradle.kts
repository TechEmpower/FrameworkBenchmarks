plugins {
    id("buildlogic.kotlin-library-conventions")
    kotlin("plugin.serialization") version libs.versions.kotlin
}

dependencies {
    // TODO move the GAVs to the version catalog too
    api(platform("io.vertx:vertx-stack-depchain:${libs.versions.vertx.get()}"))
    implementation("io.vertx:vertx-web")
    //implementation("io.netty", "netty-transport-native-epoll", classifier = "linux-x86_64")
    implementation("io.netty", "netty-transport-native-io_uring", classifier = "linux-x86_64")
    implementation("io.vertx:vertx-lang-kotlin")
    implementation("io.vertx:vertx-lang-kotlin-coroutines")

    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:${libs.versions.kotlinx.coroutines.get()}")

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:${libs.versions.kotlinx.serialization.get()}")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json-io:${libs.versions.kotlinx.serialization.get()}")
    implementation("org.jetbrains.kotlinx:kotlinx-io-core:${libs.versions.kotlinx.io.get()}")

    implementation("org.jetbrains.kotlinx:kotlinx-html:${libs.versions.kotlinx.html.get()}")
    implementation("org.jetbrains.kotlinx:kotlinx-datetime:${libs.versions.kotlinx.datetime.get()}")
}
