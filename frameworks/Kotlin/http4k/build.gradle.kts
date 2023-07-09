plugins {
    kotlin("jvm") version "1.9.0"
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

    repositories {
        mavenCentral()
    }
}
