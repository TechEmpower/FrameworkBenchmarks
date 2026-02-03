plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))
    implementation(project(":with-db:r2dbc-common"))

    implementation(libs.kotlinx.coroutines.reactive)
}

application.mainClass.set("MainKt")
