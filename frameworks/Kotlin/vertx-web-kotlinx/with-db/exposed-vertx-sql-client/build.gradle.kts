plugins {
    id("buildlogic.kotlin-application-conventions")
}

repositories {
    // TODO comment out
    mavenLocal() // for snapshot dependencies
}

dependencies {
    implementation(project(":with-db:with-db-common"))
    implementation("io.vertx:vertx-pg-client") // explicitly added to keep the version aligned

    implementation("com.huanshankeji:exposed-vertx-sql-client-core:${libs.versions.exposedVertxSqlClient.get()}")
    implementation("com.huanshankeji:exposed-vertx-sql-client-postgresql:${libs.versions.exposedVertxSqlClient.get()}")
}

application.mainClass.set("MainKt")
