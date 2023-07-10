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

tasks {
    named<Jar>("jar") {
        manifest {
            attributes["Main-Class"] = "Http4kGraalVMBenchmarkServerKt"
        }
    }
}