libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-6",
  "org.specs2" %% "specs2" % "1.14" % "test",
  "postgresql" % "postgresql" % "9.1-903.jdbc4" from "http://jdbc.postgresql.org/download/postgresql-9.1-903.jdbc4.jar"
)

scalaVersion := "2.10.1"

squeryl2scalikejdbcSettings

Squeryl2scalikejdbcKeys.outputDirectory <<= (scalaSource in Compile)(_ / "foo" / "bar")

Squeryl2scalikejdbcKeys.schema := "squeryl2scalikejdbc.test.schema$"

Squeryl2scalikejdbcKeys.useJoda := false

Squeryl2scalikejdbcKeys.scalikejdbcVersion := Some("1.5.3")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")
