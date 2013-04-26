package squeryl2scalikejdbc

import java.sql.{Types => JavaSqlTypes, Timestamp}
import org.squeryl.internals.{FieldMetaData, PosoMetaData}
import java.nio.file.{Paths, Files}
import java.io.File

object Bool{
  def unapply(str: String): Option[Boolean] =
    Some(java.lang.Boolean.valueOf(str))
}

object Main{

  def main(args: Array[String]){
    args match{
      case Array(schema, dir, Bool(useJoda)) =>
        run(Class.forName(schema), dir, useJoda)
      case _ => sys.error("invalid params " + args.mkString(" "))
    }
  }

  def run(clazz: Class[_], dir: String, useJoda: Boolean){
    invoke[Iterable[SquerylTable]](clazz, "tables").foreach{ t =>
      val conf = GeneratorConfig(useJoda = useJoda)
      val code = generateModelCode(t, conf)
      println(code)
      Files.write(Paths.get( dir + "/" + t.name + ".scala"), code.getBytes)
    }
  }

  def invoke[A](clazz: Class[_], method: String, params: (Class[_], AnyRef) *): A = {
    clazz.getMethod(method, params.map(_._1): _*).invoke(clazz.getField("MODULE$").get(null), params.map(_._2): _*).asInstanceOf[A]
  }

  type DataType = Int
  type SquerylTable = org.squeryl.Table[_]

  def fieldMetaData2Column(metadata: FieldMetaData): Column = Column(
    name = metadata.columnName,
    dataType = class2datatype(metadata.fieldType),
    rawTypeInScala = metadata.fieldType.getName,
    isNotNull = ! metadata.isOption,
    isAutoIncrement = metadata.isAutoIncremented,
    nameInScala = metadata.nameOfProperty
  )

  def squeryTable2table(table: SquerylTable): Table = {
    val fields = table.posoMetaData.fieldsMetaData
    val allColumns = fields.map(fieldMetaData2Column).toList
    Table(
      name = table.name,
      clazz = table.posoMetaData.clasz,
      allColumns = allColumns,
      primaryKeyColumns = fields.filter(_.declaredAsPrimaryKeyInSchema).map(fieldMetaData2Column).toList,
      fileName = table.name
    )
  }

  val imports = List(
    "org.joda.time._",
    "scalikejdbc._",
    "java.sql._"
  ).map("import " + ).mkString("","\n","\n\n")

  def generateModelCode(table: SquerylTable, conf: GeneratorConfig): String = {
    val clazz = table.posoMetaData.clasz
    val g = new CodeGenerator(squeryTable2table(table), clazz, clazz.getName, "_" + clazz.getSimpleName, conf)
//    imports + g.objectPart
    g.modelAll
  }

  val class2datatype: Map[Class[_], DataType] = Map[Class[_], DataType](
    classOf[Array[Any]]         -> JavaSqlTypes.ARRAY ,
    classOf[Long]               -> JavaSqlTypes.BIGINT ,
    classOf[java.lang.Long]     -> JavaSqlTypes.BIGINT ,
    classOf[Array[Byte]]        -> JavaSqlTypes.BINARY ,
    classOf[Boolean]            -> JavaSqlTypes.BOOLEAN ,
    classOf[java.lang.Boolean]  -> JavaSqlTypes.BOOLEAN ,
    classOf[String]             -> JavaSqlTypes.CHAR ,
    classOf[BigDecimal]         -> JavaSqlTypes.DECIMAL ,
    classOf[Double]             -> JavaSqlTypes.DOUBLE ,
    classOf[java.lang.Double]   -> JavaSqlTypes.DOUBLE ,
    classOf[Float]              -> JavaSqlTypes.FLOAT ,
    classOf[java.lang.Float]    -> JavaSqlTypes.FLOAT ,
    classOf[Int]                -> JavaSqlTypes.INTEGER ,
    classOf[java.lang.Integer]  -> JavaSqlTypes.INTEGER ,
    classOf[Short]              -> JavaSqlTypes.SMALLINT ,
    classOf[java.lang.Short]    -> JavaSqlTypes.SMALLINT ,
    classOf[Timestamp]          -> JavaSqlTypes.TIMESTAMP ,
    classOf[Byte]               -> JavaSqlTypes.TINYINT,
    classOf[java.lang.Byte]     -> JavaSqlTypes.TINYINT
  ).withDefaultValue(JavaSqlTypes.OTHER)

}
