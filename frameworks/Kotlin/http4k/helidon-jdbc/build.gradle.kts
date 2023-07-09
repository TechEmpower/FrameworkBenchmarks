application {
    mainClass.set("Http4kHelidonServerKt")
}

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

dependencies {
    api(project(":core-jdbc"))
    api("org.http4k:http4k-server-helidon")
}
