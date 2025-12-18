plugins {
    id("buildlogic.kotlin-library-conventions")
    kotlin("plugin.serialization") version libs.versions.kotlin
}

dependencies {
    api(project(":common"))
}
