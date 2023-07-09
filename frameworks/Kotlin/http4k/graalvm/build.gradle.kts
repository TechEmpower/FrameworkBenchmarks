application.mainClass.set("Http4kGraalVMBenchmarkServerKt")

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

dependencies {
    api(project(":core-jdbc"))
    api(project(":sunhttp"))
}

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

tasks {
    named<Jar>("jar") {
        manifest {
            attributes["Main-Class"] = "Http4kGraalVMBenchmarkServerKt"
        }
    }
}