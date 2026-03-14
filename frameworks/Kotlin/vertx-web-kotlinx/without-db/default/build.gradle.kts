plugins {
    id("buildlogic.kotlin-application-conventions")
}

dependencies {
    implementation(project(":common"))
}

application.mainClass.set("MainKt")
