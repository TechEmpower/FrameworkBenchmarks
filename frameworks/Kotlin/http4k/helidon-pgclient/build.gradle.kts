application {
    mainClass.set("http4k.Http4kHelidonServerKt")
}

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

dependencies {
    api(project(":core-pgclient"))
    api("org.http4k:http4k-server-helidon")
}
