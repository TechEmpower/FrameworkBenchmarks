plugins {
    id("buildlogic.kotlin-application-conventions")
}

repositories {
    // TODO comment out
    mavenLocal() // for snapshot dependencies
}

dependencies {
    // TODO merge code and remove these dependencies
    implementation(platform("io.vertx:vertx-stack-depchain:${libs.versions.vertx.get()}"))
    implementation("io.vertx:vertx-web")
    implementation("io.vertx:vertx-pg-client")
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

    // TODO use these below
    /*
    implementation(project(":with-db:common"))
    implementation("io.vertx:vertx-pg-client") // explicitly added to keep the version aligned
    */

    implementation("com.huanshankeji:exposed-vertx-sql-client-core:${libs.versions.exposedVertxSqlClient.get()}")
    implementation("com.huanshankeji:exposed-vertx-sql-client-postgresql:${libs.versions.exposedVertxSqlClient.get()}")
}

application.mainClass.set("MainKt")
