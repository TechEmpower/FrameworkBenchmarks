import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.gradle.api.JavaVersion.VERSION_1_8
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.9.10"
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

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

allprojects {
    apply(plugin = "kotlin")
    apply(plugin = "com.github.johnrengelman.shadow")
    apply(plugin = "application")

    repositories {
        mavenCentral()
    }

    java {
        sourceCompatibility = VERSION_1_8
        targetCompatibility = VERSION_1_8
    }

    tasks {
        withType<KotlinCompile> {
            kotlinOptions {
                jvmTarget = "1.8"
                allWarningsAsErrors = true
            }
        }

        named<ShadowJar>("shadowJar") {
            archiveBaseName.set("http4k-benchmark")
            archiveClassifier.set("")
            archiveVersion.set("")
        }
    }
}
