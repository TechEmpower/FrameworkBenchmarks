plugins {
    `kotlin-dsl`
    kotlin("jvm") version libs.versions.kotlin
}

repositories {
    gradlePluginPortal()
}

dependencies {
    implementation(libs.kotlin.gradle.plugin)
}
