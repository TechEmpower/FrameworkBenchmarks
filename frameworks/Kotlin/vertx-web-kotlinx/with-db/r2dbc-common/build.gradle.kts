plugins {
    id("buildlogic.kotlin-library-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))

    api(libs.r2dbc.spi)
    implementation(libs.r2dbc.postgresql)
    api(libs.r2dbc.pool)
}
