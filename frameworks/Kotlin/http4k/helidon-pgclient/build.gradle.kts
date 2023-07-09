application {
    mainClass.set("http4k.Http4kHelidonServerKt")
}

dependencies {
    api(project(":core-pgclient"))
    api("org.http4k:http4k-server-helidon")
}
