package com.github.xuwei_k.squeryl2scalikejdbc

import sbt._,Keys._

object Plugin extends sbt.Plugin{

  object Squeryl2scalikejdbcKeys{
    val scalikejdbcVersion = SettingKey[Option[String]]("squeryl2scalikejdbc-scalikejdbc-version")
    val dependencies = SettingKey[Seq[ModuleID]]("squeryl2scalikejdbc-dependencies")
    val outputDirectory = SettingKey[File]("squeryl2scalikejdbc-output-directory")
    val schema = SettingKey[String]("squeryl2scalikejdbc-schema")
    val useJoda = SettingKey[Boolean]("squeryl2scalikejdbc-use-joda") // TODO delete this key. and get column type info from squeryl tables
    val squeryl2scalikejdbc = TaskKey[Unit]("squeryl2scalikejdbc")
  }

  import Squeryl2scalikejdbcKeys._

  val squeryl2scalikejdbcSettings: Seq[sbt.Project.Setting[_]] = seq(
    useJoda := true,
    scalikejdbcVersion := None,
    dependencies <<= (scalikejdbcVersion, scalaBinaryVersion)((scalikejdbcV, scalaV) =>
      scalikejdbcV.map( v =>
        Seq("com.github.seratch" % ("scalikejdbc-interpolation_" + scalaV) % v)
      ).getOrElse(Nil)
    ),
    libraryDependencies <++= dependencies,
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
    squeryl2scalikejdbc <<= (schema, outputDirectory, useJoda, runner in run, fullClasspath in Compile, streams) map {
      (schema, out, useJoda, runner, classpath, streams) =>
        val param = Seq(schema, out.toString, useJoda.toString)
        runner.run("squeryl2scalikejdbc.Main", classpath.map(_.data), param, streams.log) foreach sys.error
    }
  )

}

