import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "2.1.21"
    kotlin("kapt") version "2.1.21"
    kotlin("plugin.serialization") version "2.1.21"
    id("com.gradleup.shadow") version "8.3.9"
}

group = "org.jetbrains.ktor"
version = "1.0-SNAPSHOT"

val ktorVersion = "3.3.3"
val serializationVersion = "1.8.1"
val hikariVersion = "5.1.0"
val logbackVersion = "1.5.13"
val mysqlVersion = "8.0.33"
val postgresqlVersion = "42.7.5"
val jstachioVersion = "1.3.6"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-core:$serializationVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:$serializationVersion")

    implementation("io.ktor:ktor-server-default-headers-jvm:$ktorVersion")
    implementation("io.ktor:ktor-server-netty-jvm:$ktorVersion")

    implementation("com.zaxxer:HikariCP:$hikariVersion")
    implementation("ch.qos.logback:logback-classic:$logbackVersion")

    implementation("org.postgresql:postgresql:$postgresqlVersion")
    implementation("mysql:mysql-connector-java:$mysqlVersion")

    implementation("io.jstach:jstachio:$jstachioVersion")
    kapt("io.jstach:jstachio-apt:$jstachioVersion")
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
    mainClass: String,
    exclusions: List<String>
) = tasks.register(name, ShadowJar::class) {
    archiveBaseName.set("tech-empower-framework-benchmark")
    archiveVersion.set(project.version.toString())
    archiveClassifier.set(classifier)
    manifest {
        attributes["Main-Class"] = mainClass
    }
    from(sourceSets.main.get().output)
    configurations = listOf(project.configurations.runtimeClasspath.get())
    dependencies {
        exclusions.forEach { exclude(dependency(it)) }
    }
    exclude("META-INF/*.SF")
    exclude("META-INF/*.DSA")
    exclude("META-INF/*.RSA")
    mergeServiceFiles()
}

val nettyBundle by registerBundle(
    name = "nettyBundle",
    classifier = "netty-bundle",
    mainClass = "io.ktor.server.netty.EngineMain",
    exclusions = emptyList()
)

tasks.named("build") {
    dependsOn(nettyBundle)
}
