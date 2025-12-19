plugins {
    id("buildlogic.kotlin-library-conventions")
}

dependencies {
    implementation("org.postgresql:r2dbc-postgresql:${libs.versions.r2dbcPostgresql.get()}")
}
