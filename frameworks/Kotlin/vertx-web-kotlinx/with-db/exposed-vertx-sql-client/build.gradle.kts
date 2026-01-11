plugins {
    id("buildlogic.kotlin-application-conventions")
}

repositories {
    // TODO comment out
    mavenLocal() // for snapshot dependencies
}

dependencies {
    implementation(project(":with-db:with-db-common"))
    implementation(project(":with-db:exposed-common"))

    implementation(libs.vertx.pgClient) // explicitly added to keep the version aligned
    implementation(libs.exposedVertxSqlClient.core)
    implementation(libs.exposedVertxSqlClient.postgresql)
}

application.mainClass.set("MainKt")
