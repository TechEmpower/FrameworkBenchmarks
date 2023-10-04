application.mainClass.set("Http4kGraalVMBenchmarkServerKt")

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(20))
    }
}

dependencies {
    api(project(":core-jdbc"))
    api(project(":apache"))
}

tasks {
    named<Jar>("jar") {
        manifest {
            attributes["Main-Class"] = "Http4kGraalVMBenchmarkServerKt"
        }
    }
}
