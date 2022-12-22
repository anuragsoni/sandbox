import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.7.20"
    id("com.diffplug.spotless") version "6.9.1"
    application
}

group = "com.sonianurag"

version = "1.0-SNAPSHOT"

repositories { mavenCentral() }

dependencies {
    testImplementation(kotlin("test"))
    // https://mvnrepository.com/artifact/io.netty/netty-all
    implementation("io.netty:netty-all:4.1.86.Final")

    // https://mvnrepository.com/artifact/org.jetbrains.kotlinx/kotlinx-coroutines-core
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.4")
}

tasks.test { useJUnitPlatform() }

tasks.withType<KotlinCompile> { kotlinOptions.jvmTarget = "1.8" }

application { mainClass.set("MainKt") }

spotless {
    kotlin {
        target("**/*.kt")
        ktfmt("0.39").dropboxStyle()
    }
    kotlinGradle {
        target("**/*.kts", "*.kts")
        ktfmt("0.39").dropboxStyle()
    }
}
