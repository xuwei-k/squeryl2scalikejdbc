package squeryl2scalikejdbc

import java.sql.{Types => JavaSqlTypes}
import CodeGenerator._

final case class ColumnInScala(underlying: Column) {

  lazy val typeInScala: String = {
    if (underlying.isNotNull) underlying.rawTypeInScala
    else "Option[" + underlying.rawTypeInScala + "]"
  }

  lazy val extractorName: String = underlying.dataType match {
    case JavaSqlTypes.ARRAY => "array"
    case JavaSqlTypes.BIGINT => "long"
    case JavaSqlTypes.BINARY => "bytes"
    case JavaSqlTypes.BIT => "boolean"
    case JavaSqlTypes.BLOB => "blob"
    case JavaSqlTypes.BOOLEAN => "boolean"
    case JavaSqlTypes.CHAR => "string"
    case JavaSqlTypes.CLOB => "clob"
    case JavaSqlTypes.DATALINK => "any"
    case JavaSqlTypes.DATE => "date"
    case JavaSqlTypes.DECIMAL => "bigDecimal"
    case JavaSqlTypes.DISTINCT => "any"
    case JavaSqlTypes.DOUBLE => "double"
    case JavaSqlTypes.FLOAT => "float"
    case JavaSqlTypes.INTEGER => "int"
    case JavaSqlTypes.JAVA_OBJECT => "any"
    case JavaSqlTypes.LONGVARBINARY => "bytes"
    case JavaSqlTypes.LONGVARCHAR => "string"
    case JavaSqlTypes.NULL => "any"
    case JavaSqlTypes.NUMERIC => "bigDecimal"
    case JavaSqlTypes.OTHER => "any"
    case JavaSqlTypes.REAL => "float"
    case JavaSqlTypes.REF => "ref"
    case JavaSqlTypes.SMALLINT => "short"
    case JavaSqlTypes.STRUCT => "any"
    case JavaSqlTypes.TIME => "time"
    case JavaSqlTypes.TIMESTAMP => "timestamp"
    case JavaSqlTypes.TINYINT => "byte"
    case JavaSqlTypes.VARBINARY => "bytes"
    case JavaSqlTypes.VARCHAR => "string"
    case _ => "any"
  }

  lazy val dummyValue: String = underlying.dataType match {
    case JavaSqlTypes.ARRAY => "null"
    case JavaSqlTypes.BIGINT => "1"
    case JavaSqlTypes.BINARY => "1"
    case JavaSqlTypes.BIT => "false"
    case JavaSqlTypes.BLOB => "null"
    case JavaSqlTypes.BOOLEAN => "true"
    case JavaSqlTypes.CHAR => "'abc'"
    case JavaSqlTypes.CLOB => "null"
    case JavaSqlTypes.DATALINK => "null"
    case JavaSqlTypes.DATE => "'1958-09-06'"
    case JavaSqlTypes.DECIMAL => "1"
    case JavaSqlTypes.DISTINCT => "null"
    case JavaSqlTypes.DOUBLE => "0.1"
    case JavaSqlTypes.FLOAT => "0.1"
    case JavaSqlTypes.INTEGER => "1"
    case JavaSqlTypes.JAVA_OBJECT => "null"
    case JavaSqlTypes.LONGVARBINARY => "null"
    case JavaSqlTypes.LONGVARCHAR => "'abc'"
    case JavaSqlTypes.NULL => "null"
    case JavaSqlTypes.NUMERIC => "1"
    case JavaSqlTypes.OTHER => "null"
    case JavaSqlTypes.REAL => "null"
    case JavaSqlTypes.REF => "null"
    case JavaSqlTypes.SMALLINT => "1"
    case JavaSqlTypes.STRUCT => "null"
    case JavaSqlTypes.TIME => "'12:00:00'"
    case JavaSqlTypes.TIMESTAMP => "'1958-09-06 12:00:00'"
    case JavaSqlTypes.TINYINT => "1"
    case JavaSqlTypes.VARBINARY => "null"
    case JavaSqlTypes.VARCHAR => "'abc'"
    case _ => "null"
  }

  lazy val defaultValueInScala: String = underlying.typeInScala match {
    case TypeName.AnyArray => "Array[Any]()"
    case TypeName.Long => "1L"
    case TypeName.ByteArray => "Array[Byte]()"
    case TypeName.Boolean => "false"
    case TypeName.String => "\"MyString\""
    case TypeName.LocalDate => "LocalTime.now"
    case TypeName.BigDecimal => "new java.math.BigDecimal(\"1\")"
    case TypeName.Double => "0.1D"
    case TypeName.Float => "0.1F"
    case TypeName.Int => "123"
    case TypeName.Short => "123"
    case TypeName.DateTime => "DateTime.now"
    case TypeName.Byte => "1"
    case _ => "null"
  }

}
