import java.net.URI
import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.dsl.KotlinVersion
import org.jetbrains.kotlin.gradle.internal.KaptWithoutKotlincTask

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
        apiVersion.set(KotlinVersion.KOTLIN_2_3)
        languageVersion.set(KotlinVersion.KOTLIN_2_3)
        freeCompilerArgs.addAll(
            "-Xjvm-default=all",
            "-Xlambdas=indy",
            "-Xstring-concat=indy-with-constants",
            "-Xno-call-assertions",
            "-Xno-param-assertions",
            "-Xno-receiver-assertions",
        )
    }
    jvmToolchain(25)
}

application {
    mainClass = "com.example.starter.AppKt"
}

val patchedNettyJar = file("libs/netty-patched.jar")

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

    // Netty
    implementation(files(patchedNettyJar))
    implementation(platform(libs.netty.bom)) {
        exclude(group = "io.netty", module = "netty-transport-classes-io_uring")
    }
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

val downloadPatchedNetty by tasks.registering {
    outputs.file(patchedNettyJar)
    doLast {
        val uri = URI.create("https://github.com/awmcc90/netty/releases/download/4.2-patched/netty-transport-classes-io_uring-4.2.9.Final.jar")
        logger.lifecycle("Downloading Patched Netty to ${patchedNettyJar.path}...")
        patchedNettyJar.parentFile.mkdirs()
        uri.toURL().openStream().use { input ->
            patchedNettyJar.outputStream().use { output ->
                input.copyTo(output)
            }
        }
    }
}

tasks {
    withType<KaptWithoutKotlincTask>().configureEach {
        dependsOn(downloadPatchedNetty)
    }

    compileKotlin {
        dependsOn(downloadPatchedNetty)
    }

    compileJava {
        dependsOn(downloadPatchedNetty)
    }

    register<JavaExec>("server") {
        dependsOn(this@tasks.classes)

        mainClass.set(application.mainClass.get())
        classpath = sourceSets.main.get().runtimeClasspath

        jvmArgs = listOf(
            "-server",
            "--enable-native-access=ALL-UNNAMED",
            "--sun-misc-unsafe-memory-access=allow",
            "--add-opens=java.base/java.nio=ALL-UNNAMED",
            "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
            "--add-opens=java.base/jdk.internal.misc=ALL-UNNAMED",
            "--add-opens=java.base/java.lang=ALL-UNNAMED",
            "-XX:+UnlockDiagnosticVMOptions",
            "-XX:+DebugNonSafepoints",
            "-XX:+EnableDynamicAgentLoading",
            "-XX:+PreserveFramePointer",
            "-Xms2G",
            "-Xmx2G",
            "-XX:MaxDirectMemorySize=6G",
            "-XX:+AlwaysPreTouch",
            "-XX:+UseParallelGC",
            "-XX:InitialCodeCacheSize=512m",
            "-XX:ReservedCodeCacheSize=512m",
            "-XX:+UseNUMA",
            "-XX:AutoBoxCacheMax=20000",
            "-XX:+UnlockExperimentalVMOptions",
            "-XX:+UseCompactObjectHeaders",
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
            "-Dio.netty.tryReflectionSetAccessible=true",
            "-Dio.netty.iouring.ringSize=8192",
            "-Dio.netty.iouring.cqSize=16384",
            "-Dtfb.type=basic",
        )
    }

    shadowJar {
        archiveClassifier = "fat"
        mergeServiceFiles()
    }
}
