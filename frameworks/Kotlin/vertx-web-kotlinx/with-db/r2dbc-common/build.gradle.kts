plugins {
    id("buildlogic.kotlin-library-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))

    implementation("org.postgresql:r2dbc-postgresql:${libs.versions.r2dbcPostgresql.get()}")
}
