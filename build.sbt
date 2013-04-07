scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")

resolvers ++= Seq(
  Opts.resolver.sonatypeReleases
)

organization := "com.github.xuwei-k"

version := "0.1.0-SNAPSHOT"

homepage := Some(url("https://github.com/xuwei-k/squeryl2scalikejdbc"))

licenses := Seq("MIT License" -> url("http://opensource.org/licenses/MIT"))

startYear := Some(2013)

pomExtra := (
  <developers>
    <developer>
      <id>xuwei-k</id>
      <name>Kenji Yoshida</name>
      <url>https://github.com/xuwei-k</url>
    </developer>
  </developers>
)

ScriptedPlugin.scriptedSettings

sbtPlugin := true

name := "squeryl2scalikejdbc"

description := "Scalikejdbc mapper generator from Squeryl case classes"

ScriptedPlugin.scriptedBufferLog := false

ScriptedPlugin.scriptedLaunchOpts := sys.process.javaVmArguments.filter(
  a => Seq("-Xmx","-Xms").exists(a.startsWith) || a.startsWith("-XX")
)

watchSources <++= ScriptedPlugin.sbtTestDirectory.map{ dir => (dir ***).get }
