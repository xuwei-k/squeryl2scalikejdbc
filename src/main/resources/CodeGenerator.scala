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

import java.io.File
import java.sql.{ Types => JavaSqlTypes }

object CodeGenerator{

  implicit def convertColumnToColumnInScala(column: Column): ColumnInScala = ColumnInScala(column)

  /**
   * Create directory to put the source code file if it does not exist yet.
   */
  def mkdirRecursively(file: File): Unit = {
    if (!file.getParentFile.exists) mkdirRecursively(file.getParentFile)
    if (!file.exists) file.mkdir()
  }

  def toCamelCase(s: String): String = s.split("_").toList.foldLeft("") {
    (camelCaseString, part) =>
      camelCaseString + toProperCase(part)
  }

  private def toProperCase(s: String): String = {
    if (s == null || s.trim.size == 0) ""
    else s.substring(0, 1).toUpperCase + s.substring(1).toLowerCase
  }

  type Closable = { def close() }

  def using[R <: Closable, A](resource: R)(f: R => A): A = {
    try {
      f(resource)
    } finally {
      scala.util.control.Exception.ignoring(classOf[Throwable]) apply {
        resource.close()
      }
    }
  }

  private def cast(column: Column, optional: Boolean): String = column.dataType match {
    case JavaSqlTypes.DATE if optional => ".map(_.toLocalDate)"
    case JavaSqlTypes.DATE => ".toLocalDate"
    case JavaSqlTypes.DECIMAL if optional => ".map(_.toScalaBigDecimal)"
    case JavaSqlTypes.DECIMAL => ".toScalaBigDecimal"
    case JavaSqlTypes.NUMERIC if optional => ".map(_.toScalaBigDecimal)"
    case JavaSqlTypes.NUMERIC => ".toScalaBigDecimal"
    case JavaSqlTypes.TIME if optional => ".map(_.toLocalTime)"
    case JavaSqlTypes.TIME => ".toLocalTime"
    case JavaSqlTypes.TIMESTAMP if optional => ".map(_.toDateTime)"
    case JavaSqlTypes.TIMESTAMP => ".toDateTime"
    case _ => ""
  }

  private def toClassName(table: Table): String = toCamelCase(table.name)
}

/**
 * Active Record like template generator
 */
class CodeGenerator(table: Table, specifiedClassName: Option[String] = None)(implicit config: GeneratorConfig = GeneratorConfig()) {

  import java.io.{ OutputStreamWriter, FileOutputStream}
  import CodeGenerator._

  private[this] val packageName = config.packageName
  private[this] val className = specifiedClassName.getOrElse(toClassName(table))
  private[this] val syntaxName = "[A-Z]".r.findAllIn(className).mkString.toLowerCase
  private[this] val comma = ","
  private[this] val eol = config.lineBreak.value

  case class IndentGenerator(i: Int) {
    def indent: String = " " * i * 2
  }

  implicit def convertIntToIndentGenerator(i: Int) = IndentGenerator(i)

  /**
   * Write the source code to file.
   */
  def writeModelIfNotExist(): Unit = {
    val file = new File(config.srcDir + "/" + packageName.replaceAll("\\.", "/") + "/" + className + ".scala")
    if (file.exists) {
      println("\"" + packageName + "." + className + "\"" + " already exists.")
    } else {
      mkdirRecursively(file.getParentFile)
      using(new FileOutputStream(file)) {
        fos =>
          using(new OutputStreamWriter(fos)) {
            writer =>
              writer.write(modelAll())
              println("\"" + packageName + "." + className + "\"" + " created.")
          }
      }
    }
  }

  /**
   * {{{
   * object Member {
   *   // ... as follows
   * }
   * }}}
   */
  def objectPart: String = {

    val allColumns = table.allColumns
    val pkColumns = if (table.primaryKeyColumns.size == 0) allColumns else table.primaryKeyColumns

    /**
     * {{{
     * val tableName = "member"
     * }}}
     */
    val tableName = "  val tableName = \"" + table.name + "\"" + eol

    /**
     * {{{
     * object columnNames {
     *   val id = "ID"
     *   val name = "NAME"
     *   val birthday = "BIRTHDAY"
     *   val all = Seq(id, name, birthday)
     * }
     * }}}
     */
    val columnNames = {
      """  object columnNames {
        |%valueDefinitions%
        |    val all = Seq(%valueNames%)
        |    val inSQL = all.mkString(", ")
        |  }
      """.stripMargin
        .replaceAll("%valueDefinitions%", allColumns.map {
          c => 2.indent + "val " + c.nameInScala + " = \"" + c.name + "\""
        }.mkString(eol))
        .replaceAll("%valueNames%", allColumns.map {
          c => c.nameInScala
        }.mkString(", "))
    }

    /**
     * {{{
     * val * = {
     *   import columnNames._
     *   (rs: WrappedResultSet) => Member(
     *     rs.long(id),
     *     rs.string(name),
     *     Option(rs.date(birthday)).map(_.toLocalDate)
     *   )
     * }
     * }}}
     */

    val _mapper = {
      2.indent + "(rs: WrappedResultSet) => " + (if (allColumns.size > 22) "new " else "") + className + "(" + eol +
        allColumns.map {
          c =>
            if (c.isNotNull) 3.indent + c.nameInScala + " = rs." + c.extractorName + "(" + c.nameInScala + ")" + cast(c, false)
            else 3.indent + c.nameInScala + " = " + "rs." + c.extractorName + "Opt(" + c.nameInScala + ")" + cast(c, true)
        }.mkString(comma + eol) + ")"
    }

    val mapper = {
      """  val * = {
        |    import columnNames._
        |%mapper%
        |  }
      """.stripMargin.replaceFirst("%mapper%", _mapper)
    }

    val _interpolationMapper = allColumns.map {
      c =>
        if (c.isNotNull) 2.indent + c.nameInScala + " = rs." + c.extractorName + "(" + syntaxName + "." + c.nameInScala + ")" + cast(c, false)
        else 2.indent + c.nameInScala + " = " + "rs." + c.extractorName + "Opt(" + syntaxName + "." + c.nameInScala + ")" + cast(c, true)
    }.mkString(comma + eol)

    val interpolationMapper = {
      """  def apply(%syntaxName%: ResultName[%className%])(rs: WrappedResultSet): %className% = new %className%(
        |%mapper%
        |  )
      """.stripMargin.replaceAll("%className%", className)
        .replaceFirst("%syntaxName%", syntaxName)
        .replaceFirst("%mapper%", _interpolationMapper)
    }

    /**
     * {{{
     * object joinedColumnNames {
     *   val delimiter = "__ON__"
     *   def as(name: String) = name + delimiter + tableName
     *   val id = as(columnNames.id)
     *   val name = as(columnNames.name)
     *   val birthday = as(columnNames.birthday)
     *   val all = Seq(id, name, birthday)
     *   val inSQL = columnNames.all.map(name => tableName + "." + name + " AS " + as(name)).mkString(", ")
     * }
     * }}}
     */
    val joinedColumnNames = {
      """  object joinedColumnNames {
        |    val delimiter = "__ON__"
        |    def as(name: String) = name + delimiter + tableName
        |%valueDefinitions%
        |    val all = Seq(%valueNames%)
        |    val inSQL = columnNames.all.map(name => tableName + "." + name + " AS " + as(name)).mkString(", ")
        |  }
      """.stripMargin
        .replaceAll("%valueDefinitions%", allColumns.map {
          c => 2.indent + "val " + c.nameInScala + " = as(columnNames." + c.nameInScala + ")"
        }.mkString(eol))
        .replaceAll("%valueNames%", allColumns.map {
          c => c.nameInScala
        }.mkString(", "))
    }

    /**
     * {{{
     * val joined = {
     *   import joinedColumnNames._
     *   (rs: WrappedResultSet) => Member(
     *     rs.long(id),
     *     rs.string(name),
     *     Option(rs.date(birthday)).map(_.toLocalDate)
     *   )
     * }
     * }}}
     */
    val joinedMapper = {
      """  val joined = {
        |    import joinedColumnNames._
        |%mapper%
        |  }
      """.stripMargin.replaceFirst("%mapper%", _mapper)
    }

    /**
     * {{{
     * val autoSession = AutoSession
     * }}}
     */
    val autoSession = "  val autoSession = AutoSession" + eol

    /**
     * {{{
     * def create(name: String, birthday: Option[LocalDate])(implicit session: DBSession = autoSession): Member = {
     *   val generatedKey = SQL("""
     *     insert into member (
     *       NAME,
     *       BIRTHDAY
     *     ) VALUES (
     *       /*'name*/'abc',
     *       /*'birthday*/'1958-09-06'
     *     )
     *   """).bindByName(
     *     'name -> name,
     *     'birthday -> birthday
     *   ).updateAndReturnGeneratedKey.apply()
     *
     *   Member(
     *     id = generatedKey,
     *     name = name,
     *     birthday = birthday
     *   )
     * }
     * }}}
     */
    val createMethod = {
      val createColumns: List[Column] = allColumns.filterNot {
        c => table.autoIncrementColumns.find(aic => aic.name == c.name).isDefined
      }

      val placeHolderPart: String = config.template match {
        case GeneratorTemplate.basic =>
          (1 to createColumns.size).map(c => 4.indent + "?").mkString(comma + eol)
        case GeneratorTemplate.executable =>
          createColumns.map(c => 4.indent + "/*'" + c.nameInScala + "*/" + c.dummyValue).mkString(comma + eol)
        case GeneratorTemplate.interpolation if createColumns.size <= 22 =>
          createColumns.map(c => 4.indent + "${" + c.nameInScala + "}").mkString(comma + eol)
        case _ =>
          createColumns.map(c => 4.indent + "{" + c.nameInScala + "}").mkString(comma + eol)
      }

      val bindingPart: String = config.template match {
        case GeneratorTemplate.basic =>
          3.indent + ".bind(" + eol +
            createColumns.map(c => 4.indent + c.nameInScala).mkString(comma + eol)
        case GeneratorTemplate.interpolation if createColumns.size <= 22 => ""
        case _ =>
          3.indent + ".bindByName(" + eol +
            createColumns.map {
              c => 4.indent + "'" + c.nameInScala + " -> " + c.nameInScala
            }.mkString(comma + eol)
      }

      1.indent + "def create(" + eol +
        createColumns.map {
          c => 2.indent + c.nameInScala + ": " + c.typeInScala + (if (c.isNotNull) "" else " = None")
        }.mkString(comma + eol) + ")(implicit session: DBSession = autoSession): " + className + " = {" + eol +
        2.indent + table.autoIncrementColumns.headOption.map(_ => "val generatedKey = ").getOrElse("") +
        (config.template match {
          case GeneratorTemplate.interpolation => "sql\"\"\""
          case _ => "SQL(\"\"\""
        }) + eol +
        3.indent + "insert into " +
        (config.template match {
          case GeneratorTemplate.interpolation => "${" + className + ".table}"
          case _ => table.name
        }) + " (" + eol +
        createColumns.map(c => 4.indent + c.name).mkString(comma + eol) + eol +
        3.indent + ") VALUES (" + eol +
        placeHolderPart + eol +
        3.indent + ")" + eol +
        (config.template match {
          case GeneratorTemplate.interpolation =>
            3.indent + "\"\"\"" + table.autoIncrementColumns.headOption.map(_ => ".updateAndReturnGeneratedKey.apply()").getOrElse(".update.apply()")
          case _ =>
            3.indent + "\"\"\")" + eol +
              bindingPart + eol +
              3.indent + table.autoIncrementColumns.headOption.map(_ => ").updateAndReturnGeneratedKey.apply()").getOrElse(").update.apply()")
        }) +
        eol +
        eol +
        2.indent + (if (allColumns.size > 22) "new " else "") + className + "(" + eol +
        table.autoIncrementColumns.headOption.map {
          c =>
            3.indent + c.nameInScala +
              (c.typeInScala match {
                case TypeName.Byte => " = generatedKey.toByte, "
                case TypeName.Int => " = generatedKey.toInt, "
                case TypeName.Short => " = generatedKey.toShort, "
                case TypeName.Float => " = generatedKey.toFloat, "
                case TypeName.Double => " = generatedKey.toDouble, "
                case TypeName.String => " = generatedKey.toString, "
                case TypeName.BigDecimal => " = BigDecimal.valueOf(generatedKey), "
                case _ => " = generatedKey, "
              }) + eol
        }.getOrElse("") +
        createColumns.map {
          c => 3.indent + c.nameInScala + " = " + c.nameInScala
        }.mkString(comma + eol) + ")" + eol +
        1.indent + "}" + eol
    }

    /**
     * {{{
     * def update(m: Member)(implicit session: DBSession = autoSession): Member = {
     *   SQL("""
     *     update
     *       member
     *     set
     *       ID = /*'id*/123,
     *       NAME = /*'name*/'abc',
     *       BIRTHDAY = /*'birthday*/'1958-09-06'
     *     where
     *       ID = /*'id*/123
     * """).bindByName(
     *     'id -> m.id,
     *     'name -> m.name,
     *     'birthday -> m.birthday
     *   ).update.apply()
     *   m
     * }
     * }}}
     */
    val updateMethod = {

      val placeHolderPart: String = config.template match {
        case GeneratorTemplate.basic =>
          allColumns.map(c => 4.indent + c.name + " = ?").mkString(comma + eol)
        case GeneratorTemplate.executable =>
          allColumns.map(c => 4.indent + c.name + " = /*'" + c.nameInScala + "*/" + c.dummyValue).mkString(comma + eol)
        case GeneratorTemplate.interpolation =>
          allColumns.map(c => 4.indent + c.name + " = \\${m." + c.nameInScala + "}").mkString(comma + eol)
        case _ =>
          allColumns.map(c => 4.indent + c.name + " = {" + c.nameInScala + "}").mkString(comma + eol)
      }

      val wherePart = config.template match {
        case GeneratorTemplate.basic =>
          4.indent + pkColumns.map(pk => pk.name + " = ?").mkString(" AND ")
        case GeneratorTemplate.executable =>
          4.indent + pkColumns.map(pk => pk.name + " = /*'" + pk.nameInScala + "*/" + pk.dummyValue).mkString(" AND ")
        case GeneratorTemplate.interpolation =>
          4.indent + pkColumns.map(pk => pk.name + " = \\${m." + pk.nameInScala + "}").mkString(" AND ")
        case _ =>
          4.indent + pkColumns.map(pk => pk.name + " = {" + pk.nameInScala + "}").mkString(" AND ")
      }

      val bindingPart = config.template match {
        case GeneratorTemplate.basic =>
          3.indent + ".bind(" + eol +
            allColumns.map(c => 4.indent + "m." + c.nameInScala).mkString(comma + eol) + ", " + eol +
            pkColumns.map(pk => 4.indent + "m." + pk.nameInScala).mkString(comma + eol) + eol +
            3.indent + ")"
        case GeneratorTemplate.interpolation => ""
        case _ =>
          3.indent + ".bindByName(" + eol +
            allColumns.map(c => 4.indent + "'" + c.nameInScala + " -> m." + c.nameInScala).mkString(comma + eol) + eol +
            3.indent + ")"
      }

      (config.template match {
        case GeneratorTemplate.interpolation =>
          """  def update(m: %className%)(implicit session: DBSession = autoSession): %className% = {
          |    sql%3quotes%
          |      update
          |        ${%className%.table}
          |      set
          |%placeHolderPart%
          |      where
          |%wherePart%
          |      %3quotes%.update.apply()
          |    m
          |  }
        """
        case _ =>
          """  def update(m: %className%)(implicit session: DBSession = autoSession): %className% = {
        |    SQL(%3quotes%
        |      update
        |        %tableName%
        |      set
        |%placeHolderPart%
        |      where
        |%wherePart%
        |      %3quotes%)
        |%bindingPart%.update.apply()
        |    m
        |  }
      """
      }).stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%className%", className)
        .replaceAll("%tableName%", table.name)
        .replaceAll("%placeHolderPart%", placeHolderPart)
        .replaceAll("%wherePart%", wherePart)
        .replaceAll("%bindingPart%", bindingPart)
    }

    /**
     * {{{
     * def delete(m: Member)(implicit session: DBSession = autoSession): Unit = {
     *   SQL("""delete from member where id = /*'id*/123""")
     *     .bindByName('id -> m.id)
     *     .update.apply()
     * }
     * }}}
     */
    val deleteMethod = {

      val wherePart: String = config.template match {
        case GeneratorTemplate.basic =>
          pkColumns.map(pk => pk.name + " = ?").mkString(" AND ")
        case GeneratorTemplate.executable =>
          pkColumns.map(pk => pk.name + " = /*'" + pk.nameInScala + "*/" + pk.dummyValue).mkString(" AND ")
        case GeneratorTemplate.interpolation =>
          pkColumns.map(pk => pk.name + " = \\${m." + pk.nameInScala + "}").mkString(" AND ")
        case _ =>
          pkColumns.map(pk => pk.name + " = {" + pk.nameInScala + "}").mkString(" AND ")
      }

      val bindingPart: String = config.template match {
        case GeneratorTemplate.basic =>
          ".bind(" + pkColumns.map(pk => "m." + pk.nameInScala).mkString(", ") + ")"
        case GeneratorTemplate.interpolation => ""
        case _ =>
          ".bindByName(" + pkColumns.map(pk => "'" + pk.nameInScala + " -> m." + pk.nameInScala).mkString(", ") + ")"
      }

      (config.template match {
        case GeneratorTemplate.interpolation =>
          """  def delete(m: %className%)(implicit session: DBSession = autoSession): Unit = {
          |    sql%3quotes%delete from ${%className%.table} where %wherePart%%3quotes%.update.apply()
          |  }
        """
        case _ =>
          """  def delete(m: %className%)(implicit session: DBSession = autoSession): Unit = {
            |    SQL(%3quotes%delete from %tableName% where %wherePart%%3quotes%)
            |      %bindingPart%.update.apply()
            |  }
          """
      }).stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%className%", className)
        .replaceAll("%tableName%", table.name)
        .replaceAll("%wherePart%", wherePart)
        .replaceAll("%bindingPart%", bindingPart)
    }

    /**
     * {{{
     * def find(id: Long): Option[Member] = {
     *   DB readOnly { implicit session =>
     *     SQL("""select * from member where id = /*'id*/123""")
     *       .bindByName('id -> id).map(*).single.apply()
     *   }
     * }
     * }}}
     */
    val findMethod = {
      val argsPart = pkColumns.map(pk => pk.nameInScala + ": " + pk.typeInScala).mkString(", ")
      val wherePart = (config.template match {
        case GeneratorTemplate.basic =>
          pkColumns.map(pk => pk.name + " = ?").mkString(" AND ")
        case GeneratorTemplate.executable =>
          pkColumns.map(pk => pk.name + " = /*'" + pk.nameInScala + "*/" + pk.dummyValue).mkString(" AND ")
        case GeneratorTemplate.interpolation =>
          pkColumns.map(pk => pk.name + " = \\${" + pk.nameInScala + "}").mkString(" AND ")
        case _ =>
          pkColumns.map(pk => pk.name + " = {" + pk.nameInScala + "}").mkString(" AND ")
      })
      val bindingPart = (config.template match {
        case GeneratorTemplate.basic =>
          ".bind(" + pkColumns.map(pk => pk.nameInScala).mkString(", ")
        case GeneratorTemplate.interpolation => ""
        case _ =>
          ".bindByName(" + pkColumns.map(pk => "'" + pk.nameInScala + " -> " + pk.nameInScala).mkString(", ")
      }) + ")"

      (config.template match {
        case GeneratorTemplate.interpolation =>
          """  def find(%argsPart%)(implicit session: DBSession = autoSession): Option[%className%] = {
            |    sql%3quotes%select ${%syntaxName%.result.*} from ${%className% as %syntaxName%} where %wherePart%%3quotes%
            |      .map(%className%(%syntaxName%.resultName)).single.apply()
            |  }
          """
        case _ =>
          """  def find(%argsPart%)(implicit session: DBSession = autoSession): Option[%className%] = {
            |    SQL(%3quotes%select * from %tableName% where %wherePart%%3quotes%)
            |      %bindingPart%.map(*).single.apply()
            |  }
          """
      }).stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%argsPart%", argsPart)
        .replaceAll("%className%", className)
        .replaceAll("%syntaxName%", syntaxName)
        .replaceAll("%tableName%", table.name)
        .replaceAll("%wherePart%", wherePart)
        .replaceAll("%bindingPart%", bindingPart)
    }

    /**
     * {{{
     * def countAll(): Long = {
     *   DB readOnly { implicit session =>
     *     SQL("""select count(1) from member""")
     *       .map(rs => rs.long(1)).single.apply().get
     *   }
     * }
     * }}}
     */
    val countAllMethod =
      (config.template match {
        case GeneratorTemplate.interpolation =>
          """  def countAll()(implicit session: DBSession = autoSession): Long = {
            |    sql%3quotes%select count(1) from ${%className%.table}%3quotes%.map(rs => rs.long(1)).single.apply().get
            |  }
          """
        case _ =>
          """  def countAll()(implicit session: DBSession = autoSession): Long = {
            |    SQL(%3quotes%select count(1) from %tableName%%3quotes%).map(rs => rs.long(1)).single.apply().get
            |  }
          """
      }).stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%className%", className)
        .replaceAll("%tableName%", table.name)

    /**
     * {{{
     * def findAll(): List[Member] = {
     *   DB readOnly { implicit session =>
     *     SQL("""select * from member""").map(*).list.apply()
     *   }
     * }
     * }}}
     */
    val findAllMethod =
      (config.template match {
        case GeneratorTemplate.interpolation =>
          """  def findAll()(implicit session: DBSession = autoSession): List[%className%] = {
            |    sql%3quotes%select ${%syntaxName%.result.*} from ${%className% as %syntaxName%}%3quotes%.map(%className%(%syntaxName%.resultName)).list.apply()
            |  }
          """
        case _ =>
          """  def findAll()(implicit session: DBSession = autoSession): List[%className%] = {
            |    SQL(%3quotes%select * from %tableName%%3quotes%).map(*).list.apply()
            |  }
          """
      }).stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%className%", className)
        .replaceAll("%syntaxName%", syntaxName)
        .replaceAll("%tableName%", table.name)

    /**
     * {{{
     * def findAllBy(where: String, params:(Symbol, Any)*): List[Member] = {
     *   DB readOnly { implicit session =>
     *     SQL("""select * from member """ + where)
     *       .bindByName(params: _*).map(*).list.apply()
     *   }
     * }
     * }}}
     */
    val findAllByMethod = {
      val paramsPart = (config.template match {
        case GeneratorTemplate.basic => "params: Any*"
        case _ => "params: (Symbol, Any)*"
      })
      val bindingPart = (config.template match {
        case GeneratorTemplate.basic => ".bind"
        case _ => ".bindByName"
      }) + "(params: _*)"

      """  def findAllBy(where: String, %paramsPart%)(implicit session: DBSession = autoSession): List[%className%] = {
        |    SQL(%3quotes%select * from %tableName% where %3quotes% + where)
        |      %bindingPart%.map(*).list.apply()
        |  }
      """.stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%paramsPart%", paramsPart)
        .replaceAll("%className%", className)
        .replaceAll("%tableName%", table.name)
        .replaceAll("%bindingPart%", bindingPart)
    }

    val interpolationFindAllByMethod = {
      """  def findAllBy(where: SQLSyntax)(implicit session: DBSession = autoSession): List[%className%] = {
        |    sql%3quotes%select ${%syntaxName%.result.*} from ${%className% as %syntaxName%} where ${where}%3quotes%
        |      .map(%className%(%syntaxName%.resultName)).list.apply()
        |  }
      """.stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%className%", className)
        .replaceAll("%syntaxName%", syntaxName)
    }

    /**
     * {{{
     * def countBy(where: String, params:(Symbol, Any)*): Long = {
     *   DB readOnly { implicit session =>
     *     SQL("""select count(1) from member """ + where)
     *       .bindByName(params: _*).map(*).single.apply().get
     *   }
     * }
     * }}}
     */
    val countByMethod = {
      val paramsPart = (config.template match {
        case GeneratorTemplate.basic => "params: Any*"
        case _ => "params: (Symbol, Any)*"
      })
      val bindingPart = (config.template match {
        case GeneratorTemplate.basic => ".bind"
        case _ => ".bindByName"
      }) + "(params: _*)"

      """  def countBy(where: String, %paramsPart%)(implicit session: DBSession = autoSession): Long = {
        |    SQL(%3quotes%select count(1) from %tableName% where %3quotes% + where)
        |      %bindingPart%.map(rs => rs.long(1)).single.apply().get
        |  }
      """.stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%paramsPart%", paramsPart)
        .replaceAll("%className%", className)
        .replaceAll("%tableName%", table.name)
        .replaceAll("%bindingPart%", bindingPart)
    }

    val interpolationCountByMethod = {
      """  def countBy(where: SQLSyntax)(implicit session: DBSession = autoSession): Long = {
        |    sql%3quotes%select count(1) from ${%className% as %syntaxName%} where ${where}%3quotes%
        |      .map(_.long(1)).single.apply().get
        |  }
      """.stripMargin
        .replaceAll("%3quotes%", "\"\"\"")
        .replaceAll("%className%", className)
        .replaceAll("%syntaxName%", syntaxName)
    }

    val isInterpolation = config.template == GeneratorTemplate.interpolation
    "object " + className + (if (isInterpolation) " extends SQLSyntaxSupport[" + className + "]" else "") + " {" + eol +
      eol +
      (if (isInterpolation) 1.indent + "override val tableName = \"" + table.name + "\"" + eol else tableName) +
      eol +
      (if (isInterpolation) 1.indent + "override val columns = Seq(" + allColumns.map(c => c.name).mkString("\"", "\", \"", "\"") + ")" + eol else columnNames) +
      eol +
      (if (isInterpolation) interpolationMapper else mapper) +
      eol +
      (if (isInterpolation) "" else joinedColumnNames + eol) +
      (if (isInterpolation) "" else joinedMapper + eol) +
      (if (isInterpolation) 1.indent + "val " + syntaxName + " = " + className + ".syntax(\"" + syntaxName + "\")" + eol + eol else "") +
      autoSession +
      eol +
      findMethod +
      eol +
      findAllMethod +
      eol +
      countAllMethod +
      eol +
      (if (isInterpolation) interpolationFindAllByMethod else findAllByMethod) +
      eol +
      (if (isInterpolation) interpolationCountByMethod else countByMethod) +
      eol +
      createMethod +
      eol +
      updateMethod +
      eol +
      deleteMethod +
      eol +
      "}"
  }

  def modelAll(): String = {
    val jodaTimeImport = table.allColumns.flatMap {
      c =>
        c.rawTypeInScala match {
          case TypeName.DateTime => Some("DateTime")
          case TypeName.LocalDate => Some("LocalDate")
          case TypeName.LocalTime => Some("LocalTime")
          case _ => None
        }
    } match {
      case classes if classes.size > 0 => "import org.joda.time.{" + classes.distinct.mkString(", ") + "}" + eol
      case _ => ""
    }
    val javaSqlImport = table.allColumns.flatMap {
      c =>
        c.rawTypeInScala match {
          case TypeName.Blob => Some("Blob")
          case TypeName.Clob => Some("Clob")
          case TypeName.Ref => Some("Ref")
          case TypeName.Struct => Some("Struct")
          case _ => None
        }
    } match {
      case classes if classes.size > 0 => "import java.sql.{" + classes.distinct.mkString(", ") + "}" + eol
      case _ => ""
    }
    "package " + config.packageName + eol +
      eol +
      "import scalikejdbc._" + eol +
      (config.template match {
        case GeneratorTemplate.interpolation => "import scalikejdbc.SQLInterpolation._" + eol
        case _ => ""
      }) +
      jodaTimeImport +
      javaSqlImport +
      eol +
      objectPart + eol
  }

  def specAll(): Option[String] =
    new Spec(table, packageName, className, config).specAll()
}

