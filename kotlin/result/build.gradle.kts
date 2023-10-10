plugins {
    kotlin("jvm") version "1.9.0"
    id("com.diffplug.spotless") version "6.22.0"
    application
}

group = "io.github.anuragsoni"

version = "1.0-SNAPSHOT"

repositories { mavenCentral() }

dependencies { testImplementation(kotlin("test")) }

tasks.test { useJUnitPlatform() }

kotlin { jvmToolchain(17) }

application { mainClass.set("MainKt") }

spotless {
    kotlin { ktfmt("0.46").kotlinlangStyle() }

    kotlinGradle { ktfmt("0.46").kotlinlangStyle() }
}
