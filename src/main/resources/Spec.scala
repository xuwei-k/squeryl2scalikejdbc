/*
 * Copyright 2012 Kazuhiro Sera
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package squeryl2scalikejdbc

import java.io.{File, FileOutputStream, OutputStreamWriter}
import CodeGenerator._

class Spec(table: Table, packageName: String, className: String, config: GeneratorConfig){

  def writeSpecIfNotExist(code: Option[String]): Unit = {
    val file = new File(config.testDir + "/" + packageName.replaceAll("\\.", "/") + "/" + className + "Spec.scala")
    if (file.exists) {
      println("\"" + packageName + "." + className + "Spec\"" + " already exists.")
    } else {
      code.map { code =>
        mkdirRecursively(file.getParentFile)
        using(new FileOutputStream(file)) {
          fos =>
            using(new OutputStreamWriter(fos)) {
              writer =>
                writer.write(code)
                println("\"" + packageName + "." + className + "Spec\"" + " created.")
            }
        }
      }
    }
  }

  def specAll(): Option[String] = config.testTemplate match {
    case GeneratorTestTemplate.ScalaTestFlatSpec =>
      Some(replaceVariablesForTestPart(
        """package %package%
          |
          |import org.scalatest._
          |import org.joda.time._
          |import scalikejdbc.scalatest.AutoRollback
          |%interpolationImport%
          |
          |class %className%Spec extends fixture.FlatSpec with ShouldMatchers with AutoRollback {
          |
          |  behavior of "%className%"
          |
          |  it should "find by primary keys" in { implicit session =>
          |    val maybeFound = %className%.find(%primaryKeys%)
          |    maybeFound.isDefined should be(true)
          |  }
          |  it should "find all records" in { implicit session =>
          |    val allResults = %className%.findAll()
          |    allResults.size should be >(0)
          |  }
          |  it should "count all records" in { implicit session =>
          |    val count = %className%.countAll()
          |    count should be >(0L)
          |  }
          |  it should "find by where clauses" in { implicit session =>
          |    val results = %className%.findAllBy(%whereExample%)
          |    results.size should be >(0)
          |  }
          |  it should "count by where clauses" in { implicit session =>
          |    val count = %className%.countBy(%whereExample%)
          |    count should be >(0L)
          |  }
          |  it should "create new record" in { implicit session =>
          |    val created = %className%.create(%createFields%)
          |    created should not be(null)
          |  }
          |  it should "update a record" in { implicit session =>
          |    val entity = %className%.findAll().head
          |    val updated = %className%.update(entity)
          |    updated should not equal(entity)
          |  }
          |  it should "delete a record" in { implicit session =>
          |    val entity = %className%.findAll().head
          |    %className%.delete(entity)
          |    val shouldBeNone = %className%.find(%primaryKeys%)
          |    shouldBeNone.isDefined should be(false)
          |  }
          |
          |}
        """.stripMargin))
    case GeneratorTestTemplate.specs2unit =>
      Some(replaceVariablesForTestPart(
        """package %package%
          |
          |import scalikejdbc.specs2.mutable.AutoRollback
          |import org.specs2.mutable._
          |import org.joda.time._
          |%interpolationImport%
          |
          |class %className%Spec extends Specification {
          |
          |  "%className%" should {
          |    "find by primary keys" in new AutoRollback {
          |      val maybeFound = %className%.find(%primaryKeys%)
          |      maybeFound.isDefined should beTrue
          |    }
          |    "find all records" in new AutoRollback {
          |      val allResults = %className%.findAll()
          |      allResults.size should be_>(0)
          |    }
          |    "count all records" in new AutoRollback {
          |      val count = %className%.countAll()
          |      count should be_>(0L)
          |    }
          |    "find by where clauses" in new AutoRollback {
          |      val results = %className%.findAllBy(%whereExample%)
          |      results.size should be_>(0)
          |    }
          |    "count by where clauses" in new AutoRollback {
          |      val count = %className%.countBy(%whereExample%)
          |      count should be_>(0L)
          |    }
          |    "create new record" in new AutoRollback {
          |      val created = %className%.create(%createFields%)
          |      created should not beNull
          |    }
          |    "update a record" in new AutoRollback {
          |      val entity = %className%.findAll().head
          |      val updated = %className%.update(entity)
          |      updated should not equalTo(entity)
          |    }
          |    "delete a record" in new AutoRollback {
          |      val entity = %className%.findAll().head
          |      %className%.delete(entity)
          |      val shouldBeNone = %className%.find(%primaryKeys%)
          |      shouldBeNone.isDefined should beFalse
          |    }
          |  }
          |
          |}
        """.stripMargin))
    case GeneratorTestTemplate.specs2acceptance =>
      Some(replaceVariablesForTestPart(
        """package %package%
          |
          |import scalikejdbc.specs2.AutoRollback
          |import org.specs2._
          |import org.joda.time._
          |%interpolationImport%
          |
          |class %className%Spec extends Specification { def is =
          |
          |  "The '%className%' model should" ^
          |    "find by primary keys"         ! autoRollback().findByPrimaryKeys ^
          |    "find all records"             ! autoRollback().findAll ^
          |    "count all records"            ! autoRollback().countAll ^
          |    "find by where clauses"        ! autoRollback().findAllBy ^
          |    "count by where clauses"       ! autoRollback().countBy ^
          |    "create new record"            ! autoRollback().create ^
          |    "update a record"              ! autoRollback().update ^
          |    "delete a record"              ! autoRollback().delete ^
          |                                   end
          |
          |  case class autoRollback() extends AutoRollback {
          |
          |    def findByPrimaryKeys = this {
          |      val maybeFound = %className%.find(%primaryKeys%)
          |      maybeFound.isDefined should beTrue
          |    }
          |    def findAll = this {
          |      val allResults = %className%.findAll()
          |      allResults.size should be_>(0)
          |    }
          |    def countAll = this {
          |      val count = %className%.countAll()
          |      count should be_>(0L)
          |    }
          |    def findAllBy = this {
          |      val results = %className%.findAllBy(%whereExample%)
          |      results.size should be_>(0)
          |    }
          |    def countBy = this {
          |      val count = %className%.countBy(%whereExample%)
          |      count should be_>(0L)
          |    }
          |    def create = this {
          |      val created = %className%.create(%createFields%)
          |      created should not beNull
          |    }
          |    def update = this {
          |      val entity = %className%.findAll().head
          |      val updated = %className%.update(entity)
          |      updated should not equalTo(entity)
          |    }
          |    def delete = this {
          |      val entity = %className%.findAll().head
          |      %className%.delete(entity)
          |      val shouldBeNone = %className%.find(%primaryKeys%)
          |      shouldBeNone.isDefined should beFalse
          |    }
          |  }
          |
          |}
        """.stripMargin))
    case GeneratorTestTemplate(name) => None
  }

  private def replaceVariablesForTestPart(code: String): String = {
    val isInterpolation = config.template == GeneratorTemplate.interpolation
    code.replaceAll("%package%", packageName)
      .replaceAll("%className%", className)
      .replaceFirst("%interpolationImport%", if (isInterpolation) "import scalikejdbc.SQLInterpolation._" else "")
      .replaceAll("%primaryKeys%", table.primaryKeyColumns.map {
        c => c.defaultValueInScala
      }.mkString(", "))
      .replaceAll("%whereExample%",
        if (isInterpolation) table.primaryKeyColumns.headOption.map(c =>
          "sqls\"" + c.name + " = \\${" + c.defaultValueInScala + "}\"").getOrElse("")
        else table.primaryKeyColumns.headOption.map(c =>
          "\"" + c.name + " = {" + c.nameInScala + "}\", '" + c.nameInScala + " -> " + c.defaultValueInScala).getOrElse("")
      )
      .replaceAll("%createFields%", table.allColumns.filter {
        c =>
          c.isNotNull && table.autoIncrementColumns.find(aic => aic.name == c.name).isEmpty
      }.map {
        c =>
          c.nameInScala + " = " + c.defaultValueInScala
      }.mkString(", "))
  }

}

