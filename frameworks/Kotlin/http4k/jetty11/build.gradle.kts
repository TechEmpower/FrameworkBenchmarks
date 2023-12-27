application.mainClass.set("Http4kJettyServerKt")

dependencies {
    api(project(":core-jdbc"))
    api("org.http4k:http4k-server-jetty11")
}