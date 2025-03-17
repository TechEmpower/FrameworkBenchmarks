pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

plugins {
    id("org.gradle.toolchains.foojay-resolver") version "0.4.0"
}

toolchainManagement {
    jvm {
        javaRepositories {
            repository("foojay") {
                resolverClass.set(org.gradle.toolchains.foojay.FoojayToolchainResolver::class.java)
            }
        }
    }
}

rootProject.name = "http4k-benchmark"
include("core")
include("core-jdbc")
include("core-pgclient")


include("apache")
include("apache4")
include("jetty")
include("jettyloom-jdbc")
include("helidon-jdbc")
include("helidon-pgclient")
include("ktornetty")
include("netty")
include("ratpack")
include("undertow")
