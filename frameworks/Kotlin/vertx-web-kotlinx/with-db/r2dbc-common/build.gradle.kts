plugins {
    id("buildlogic.kotlin-library-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))

    implementation(libs.r2dbc.postgresql)
}
