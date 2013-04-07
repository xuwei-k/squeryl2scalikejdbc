package com.github.xuwei_k.squeryl2scalikejdbc

import sbt._,Keys._

object Plugin extends sbt.Plugin{

  object Squeryl2scalikejdbcKeys{
    val squerylVersion = SettingKey[String]("squeryl2scalikejdbc-squeryl-version")
    val scalikejdbcVersion = SettingKey[String]("squeryl2scalikejdbc-scalikejdbc-version")
    val scalikejdbcDependency = SettingKey[ModuleID]("squeryl2scalikejdbc-scalikejdbc-dependency")
    val squerylDependency = SettingKey[ModuleID]("squeryl2scalikejdbc-squeryl-dependency")
    val dependencies = SettingKey[Seq[ModuleID]]("squeryl2scalikejdbc-dependencies")
    val outputDirectory = SettingKey[File]("squeryl2scalikejdbc-output-directory")
    val schema = SettingKey[String]("squeryl2scalikejdbc-schema")
    val squeryl2scalikejdbc = TaskKey[Unit]("squeryl2scalikejdbc")
  }

  import Squeryl2scalikejdbcKeys._

  val squeryl2scalikejdbcSettings: Seq[sbt.Project.Setting[_]] = seq(
    squerylDependency <<= (squerylVersion, scalaBinaryVersion)((v, scalaV) =>
      "org.squeryl" % ("squeryl_" + scalaV) % v
    ),
    scalikejdbcDependency <<= (scalikejdbcVersion, scalaBinaryVersion)((v, scalaV) =>
      "com.github.seratch" % ("scalikejdbc_" + scalaV) % v
    ),
    dependencies := Nil,
    dependencies <+= squerylDependency,
    dependencies <+= scalikejdbcDependency,
    libraryDependencies <++= Squeryl2scalikejdbcKeys.dependencies,
    sourceGenerators in Compile <+= (sourceManaged in Compile){ dir => task{
      Seq(
        "CodeGenerator", "Column", "GeneratorConfig", "Main", "Table", "Spec", "ColumnInScala",
        "TypeName"
      ).map("/" + _ + ".scala").map{ src =>
        val in = this.getClass.getResourceAsStream(src)
        val f = dir / ("squeryl2scalikejdbc" + src)
        IO.transfer(in, f)
        f
      }
    }},
    squeryl2scalikejdbc <<= (schema, outputDirectory, runner in run, fullClasspath in Compile, streams) map {
      (schema, out, runner, classpath, streams) =>
        runner.run("squeryl2scalikejdbc.Main", classpath.map(_.data), Seq(schema, out.toString), streams.log) foreach sys.error
    }
  )

}

