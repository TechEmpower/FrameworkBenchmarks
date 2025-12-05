import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "2.1.21"
    kotlin("plugin.serialization") version "2.1.21"
    id("com.gradleup.shadow") version "8.3.9"
}

group = "org.jetbrains.ktor"
version = "1.0-SNAPSHOT"

val ktorVersion = "3.3.3"
val serializationVersion = "1.8.1"
val kotlinxHtmlVersion = "0.12.0"
val coroutinesVersion = "1.10.1"
val logbackVersion = "1.5.13"
val reactorVersion = "3.8.0"
val r2dbcPstgrsVersion = "1.1.1.RELEASE"
val r2dbcPoolVersion = "1.0.2.RELEASE"
val postgresqlVersion = "42.7.5"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("reflect"))

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:$serializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$serializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json-io:$serializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-html-jvm:$kotlinxHtmlVersion")

    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:$coroutinesVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-reactor:$coroutinesVersion")

    implementation("io.ktor:ktor-server-default-headers-jvm:$ktorVersion")
    implementation("io.ktor:ktor-server-html-builder-jvm:$ktorVersion")
    implementation("io.ktor:ktor-server-netty-jvm:$ktorVersion")

    implementation("org.postgresql:r2dbc-postgresql:$r2dbcPstgrsVersion")
    implementation("io.r2dbc:r2dbc-pool:$r2dbcPoolVersion")
    implementation("io.projectreactor:reactor-core:$reactorVersion")

    implementation("ch.qos.logback:logback-classic:$logbackVersion")
}

sourceSets {
    main {
        java.srcDirs("src/main/kotlin")
    }
}

kotlin {
    jvmToolchain(21)
}

tasks.withType<KotlinCompile>().configureEach {
    compilerOptions {
        jvmTarget.set(JvmTarget.JVM_21)
    }
}

tasks.named<ShadowJar>("shadowJar") {
    enabled = false
}

fun registerBundle(
    name: String,
    classifier: String,
    mainClass: String
) = tasks.register(name, ShadowJar::class) {
    archiveBaseName.set("tech-empower-framework-benchmark")
    archiveVersion.set(project.version.toString())
    archiveClassifier.set(classifier)
    manifest {
        attributes["Main-Class"] = mainClass
    }
    from(sourceSets.main.get().output)
    configurations = listOf(project.configurations.runtimeClasspath.get())
}

val nettyBundle by registerBundle(
    name = "nettyBundle",
    classifier = "netty-bundle",
    mainClass = "io.ktor.server.netty.EngineMain"
)

tasks.register("bundleAll") {
    description = "Builds the runnable Netty uber-jar."
    dependsOn(nettyBundle)
}

tasks.named("build") {
    dependsOn(nettyBundle)
}

