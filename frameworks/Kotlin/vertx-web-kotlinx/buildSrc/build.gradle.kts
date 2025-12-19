plugins {
    `kotlin-dsl`
    // to resolve the `Kotlin does not yet support 25 JDK target, falling back to Kotlin JVM_24 JVM target` warning
    kotlin("jvm") version libs.versions.kotlin
}

repositories {
    gradlePluginPortal()
}

dependencies {
    implementation(libs.kotlin.gradle.plugin)
}
