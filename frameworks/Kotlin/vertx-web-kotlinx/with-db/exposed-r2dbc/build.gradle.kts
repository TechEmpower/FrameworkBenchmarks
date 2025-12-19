plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))
    implementation(project(":with-db:r2dbc-common"))
    implementation(project(":with-db:exposed-common"))

    api("org.jetbrains.exposed:exposed-r2dbc:${libs.versions.exposed.get()}")
}

application.mainClass.set("MainKt")
