plugins {
    id("buildlogic.kotlin-library-conventions")
    alias(libs.plugins.kotlin.plugin.serialization)
}

dependencies {
    api(platform(libs.vertx.stack.depchain))
    api(libs.vertx.web)
    //runtimeOnly("io.netty", "netty-transport-native-epoll", classifier = "linux-x86_64")
    runtimeOnly(libs.netty.transport.native.iouring) { artifact { classifier = "linux-x86_64" } }
    api(libs.vertx.lang.kotlin)
    api(libs.vertx.lang.kotlin.coroutines)

    implementation(libs.kotlinx.coroutines.core)

    api(libs.kotlinx.serialization.json)
    implementation(libs.kotlinx.serialization.json.io)
    implementation(libs.kotlinx.io.core)

    implementation(libs.kotlinx.datetime)
}
