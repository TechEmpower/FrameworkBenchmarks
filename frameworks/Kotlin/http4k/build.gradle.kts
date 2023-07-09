import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    kotlin("jvm") version "1.9.0"
    application
}

buildscript {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }

    dependencies {
        classpath("com.github.johnrengelman:shadow:8.1.1")
    }
}

allprojects {
    apply(plugin = "kotlin")
    apply(plugin = "com.github.johnrengelman.shadow")
    apply(plugin = "application")

    repositories {
        mavenCentral()
    }

    tasks {
        named<ShadowJar>("shadowJar") {
            archiveBaseName.set("http4k-benchmark")
            archiveClassifier.set("")
            archiveVersion.set("")
        }
    }
}
