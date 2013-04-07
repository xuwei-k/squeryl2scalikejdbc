libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-6",
  "org.specs2" %% "specs2" % "1.14" % "test",
  "com.h2database" % "h2" % "1.3.171" % "test"
)

scalaVersion := "2.10.1"

Squeryl2scalikejdbcKeys.scalikejdbcVersion := "1.5.2"

Squeryl2scalikejdbcKeys.outputDirectory <<= scalaSource in Compile

Squeryl2scalikejdbcKeys.schema := "squeryl2scalikejdbc.test.schema$"

squeryl2scalikejdbcSettings

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")
