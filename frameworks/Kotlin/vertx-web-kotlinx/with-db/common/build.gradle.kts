plugins {
    id("buildlogic.kotlin-library-conventions")
    alias(libs.plugins.kotlin.plugin.serialization)
}

dependencies {
    api(project(":common"))

    implementation("org.jetbrains.kotlinx:kotlinx-html:${libs.versions.kotlinx.html.get()}")
}
