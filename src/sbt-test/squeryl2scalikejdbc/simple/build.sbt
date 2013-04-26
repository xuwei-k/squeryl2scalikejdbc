libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-6",
  "org.specs2" %% "specs2" % "1.14" % "test",
  "postgresql" % "postgresql" % "9.1-903.jdbc4" from "http://jdbc.postgresql.org/download/postgresql-9.1-903.jdbc4.jar"
)

scalaVersion := "2.10.1"

Squeryl2scalikejdbcKeys.scalikejdbcVersion := "1.5.3"

Squeryl2scalikejdbcKeys.outputDirectory <<= scalaSource in Compile

Squeryl2scalikejdbcKeys.schema := "squeryl2scalikejdbc.test.schema$"

squeryl2scalikejdbcSettings

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")
