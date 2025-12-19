plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))
    implementation(project(":with-db:r2dbc-common"))

    implementation("io.r2dbc:r2dbc-spi:${libs.versions.r2dbc.spi.get()}")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-reactive:${libs.versions.kotlinx.coroutines.get()}")
}

application.mainClass.set("MainKt")
