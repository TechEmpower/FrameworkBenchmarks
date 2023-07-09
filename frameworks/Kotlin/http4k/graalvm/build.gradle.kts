import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    application
}

dependencies {
    api(project(":core-jdbc"))
    api(project(":sunhttp"))
}

apply(plugin = "application")

application {
    mainClass.set("http4k.Http4kGraalVMBenchmarkServerKt")
}

apply(plugin = "com.github.johnrengelman.shadow")

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

tasks {
    named<Jar>("jar") {
        manifest {
            attributes["Main-Class"] = "http4k.Http4kGraalVMBenchmarkServerKt"
        }
    }

    named<ShadowJar>("shadowJar") {
        archiveBaseName.set("http4k-benchmark")
        archiveClassifier.set("")
        archiveVersion.set("")
    }
}
