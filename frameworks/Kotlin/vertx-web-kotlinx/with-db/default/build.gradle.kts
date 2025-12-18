plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":with-db:common"))
    implementation("io.vertx:vertx-pg-client")
}

application.mainClass.set("MainKt")
