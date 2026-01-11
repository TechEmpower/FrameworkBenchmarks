plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":with-db:with-db-common"))

    implementation(libs.vertx.pgClient)
}

application.mainClass.set("MainKt")
