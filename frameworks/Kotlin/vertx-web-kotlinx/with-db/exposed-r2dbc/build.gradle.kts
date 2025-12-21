plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))
    implementation(project(":with-db:r2dbc-common"))
    implementation(project(":with-db:exposed-common"))

    implementation(libs.exposed.r2dbc)
}

application.mainClass.set("MainKt")
