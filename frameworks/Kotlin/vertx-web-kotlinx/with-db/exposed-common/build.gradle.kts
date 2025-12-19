plugins {
    id("buildlogic.kotlin-library-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))

    api("org.jetbrains.exposed:exposed-core:${libs.versions.exposed.get()}")
    // explicitly made `implementation` instead of `api` to avoid importing wrong APIs in dependent projects
    implementation("org.jetbrains.exposed:exposed-jdbc:${libs.versions.exposed.get()}")
    implementation("org.jetbrains.exposed:exposed-r2dbc:${libs.versions.exposed.get()}")
}
