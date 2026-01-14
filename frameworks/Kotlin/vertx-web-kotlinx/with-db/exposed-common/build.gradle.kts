plugins {
    id("buildlogic.kotlin-library-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))

    api(libs.exposed.core)
    // explicitly made `implementation` instead of `api` to avoid importing wrong APIs in dependent projects
    implementation(libs.exposed.jdbc)
    implementation(libs.exposed.r2dbc)
}
