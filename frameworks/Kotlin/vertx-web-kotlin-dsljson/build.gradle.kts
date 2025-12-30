import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.kapt)
    alias(libs.plugins.shadow)
    application
}

group = "com.example"
version = "1.0.0-SNAPSHOT"

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(25))
    }
}

kotlin {
    compilerOptions {
        jvmTarget = JvmTarget.JVM_25
        apiVersion.set(org.jetbrains.kotlin.gradle.dsl.KotlinVersion.KOTLIN_2_3)
        languageVersion.set(org.jetbrains.kotlin.gradle.dsl.KotlinVersion.KOTLIN_2_3)
        freeCompilerArgs.addAll(listOf(
            "-Xjvm-default=all",
            "-Xlambdas=indy",
            "-Xjdk-release=25"
        ))
    }
    jvmToolchain(25)
}

application {
    mainClass = "com.example.starter.AppKt"
}

dependencies {
    // Kotlin
    implementation(libs.kotlin.stdlib)
    implementation(libs.kotlin.reflect)

    // Vert.x
    implementation(platform(libs.vertx.bom))
    implementation(libs.vertx.core)
    implementation(libs.vertx.web)
    implementation(libs.vertx.pg.client)
    implementation(libs.vertx.lang.kotlin)
    implementation(libs.vertx.lang.kotlin.coroutines)
    implementation(libs.vertx.micrometer)

    // Micrometer
    implementation(libs.micrometer.registry.prometheus)

    // Netty
    implementation(platform(libs.netty.bom))
    resolvePlatformSpecificNettyDependencies(libs.versions.netty.get())
        .forEach { implementation(it) }

    // DSL-JSON
    implementation(libs.dsl.json)
    kapt(libs.dsl.json)

    // Log4j
    implementation(libs.log4j.core)
    implementation(libs.log4j.api)
    implementation(libs.log4j.api.kotlin)
    implementation(libs.disruptor)
}

tasks {
    register<JavaExec>("server") {
        dependsOn(this@tasks.classes)

        mainClass.set(application.mainClass.get())
        classpath = sourceSets.main.get().runtimeClasspath

        jvmArgs = listOf(
            "-server",
            "--enable-native-access=ALL-UNNAMED",
            "--add-opens=java.base/java.lang=ALL-UNNAMED",
            "--sun-misc-unsafe-memory-access=allow",
            "-Xms2G",
            "-Xmx2G",
            "-XX:+AlwaysPreTouch",
            "-XX:+UseParallelGC",
            "-XX:InitialCodeCacheSize=512m",
            "-XX:ReservedCodeCacheSize=512m",
            "-XX:MaxInlineLevel=20",
            "-XX:+UseNUMA",
            "-XX:-UseCodeCacheFlushing",
            "-XX:AutoBoxCacheMax=10001",
            "-XX:+UseCompactObjectHeaders",
            "-XX:+UnlockDiagnosticVMOptions",
            "-XX:+DebugNonSafepoints",
            "-Djava.net.preferIPv4Stack=true",
            "-Dvertx.disableMetrics=true",
            "-Dvertx.disableWebsockets=true",
            "-Dvertx.disableContextTimings=true",
            "-Dvertx.cacheImmutableHttpResponseHeaders=true",
            "-Dvertx.internCommonHttpRequestHeadersToLowerCase=true",
            "-Dvertx.disableHttpHeadersValidation=true",
            "-Dio.netty.noUnsafe=false",
            "-Dio.netty.buffer.checkBounds=false",
            "-Dio.netty.buffer.checkAccessible=false",
            "-Dio.netty.leakDetection.level=disabled",
            "-Dio.netty.iouring.ringSize=4096",
            "-Dio.netty.iouring.cqSize=8192",
            "-Dtfb.type=basic",
        )
    }

    shadowJar {
        archiveClassifier = "fat"
        mergeServiceFiles()
    }
}
