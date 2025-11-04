import sbt.Keys.libraryDependencies
import sbtassembly.AssemblyPlugin.autoImport.assemblyMergeStrategy

import scala.collection.Seq
import scala.scalanative.build.{GC, LTO, Mode}

ThisBuild / scalaVersion := "3.3.7"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "solutions.s4y"

val http4sVersion = "0.23.32"
val scribeVersion = "3.13.2" // fixed version for Scala native compatibility

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .crossType(
    CrossType.Pure
  )
  .in(file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-ember-client" % http4sVersion,
      "org.http4s" %%% "http4s-circe" % http4sVersion,
      "io.circe" %%% "circe-core" % "0.14.8",
      "io.circe" %%% "circe-parser" % "0.14.8",
      "io.circe" %%% "circe-generic" % "0.14.8",
      "com.outr" %% "scribe" % scribeVersion,
      "com.outr" %%% "scribe-cats" % scribeVersion,
      "org.scalameta" %% "munit" % "1.2.1" % Test
    ),
    Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
  )

lazy val cli = crossProject(JVMPlatform, NativePlatform)
  .crossType(
    CrossType.Pure
  )
  .in(file("cli"))
  .dependsOn(core)
  .settings(
    name := "cli",
    nativeConfig ~= { c =>
      c.withLTO(LTO.full) // full, thin, none
        .withMode(Mode.releaseSize) // releaseFast, releaseSize, releaseFull
        .withGC(GC.none) // commix, immix, boehm, none
      // .withMultithreading(true) // Enable parallelism on Native
    }
  )
  .jvmSettings(
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.20",
    assemblyMergeStrategy := {
      case "module-info.class"      => MergeStrategy.discard
      case PathList("META-INF", _*) =>
        MergeStrategy.discard // Handles other common META-INF conflicts
      case x => assemblyMergeStrategy.value(x) // Fallback to default
    }
  )

lazy val httpServer = crossProject(JVMPlatform, NativePlatform)
  .crossType(
    CrossType.Pure
  )
  .in(file("http-server"))
  .dependsOn(core)
  .settings(
    name := "http-server",
    nativeConfig ~= { c =>
      c.withLTO(LTO.none) // full, thin
        .withMode(Mode.debug) // releaseFast, releaseSize, releaseFull
        .withGC(GC.commix) // commix, immix, boehm, none
      // .withMultithreading(true) // Enable parallelism on Native
    },
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-ember-server" % http4sVersion,
      "org.http4s" %%% "http4s-dsl" % http4sVersion,
      "com.monovore" %%% "decline" % "2.4.1",
      "org.scalameta" %% "munit" % "1.2.1" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.1.0" % Test
    )
  )
  .jvmSettings(
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.20",
    assemblyMergeStrategy := {
      case "module-info.class"      => MergeStrategy.discard
      case PathList("META-INF", _*) =>
        MergeStrategy.discard // Handles other common META-INF conflicts
      case x => assemblyMergeStrategy.value(x) // Fallback to default
    }
  )
