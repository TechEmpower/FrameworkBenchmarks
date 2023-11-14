application.mainClass.set("Http4kGraalVMBenchmarkServerKt")

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
