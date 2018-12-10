import Dependencies._

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.regiskuckaertz",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions ++= Seq(
        "-Ypartial-unification",
        "-language:higherKinds",
        "-feature",
        "-deprecation"
      )
    )),
    name := "Cofree",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M26",
    libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.3.2",
    libraryDependencies += scalaTest % Test
  )
