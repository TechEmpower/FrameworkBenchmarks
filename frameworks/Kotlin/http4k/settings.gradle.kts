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
include("apache-graalvm")
include("apache4")
include("graalvm")
include("jetty")
include("jettyloom-jdbc")
include("jettyloom-pgclient")
include("jetty11")
include("jetty11loom-jdbc")
include("jetty11loom-pgclient")
include("helidon-jdbc")
include("helidon-pgclient")
include("helidon-graalvm")
include("ktorcio")
include("ktornetty")
include("netty")
include("ratpack")
include("sunhttp")
include("sunhttploom")
include("undertow")
