package squeryl2scalikejdbc.test

import org.squeryl.{Schema, KeyedEntity}
import org.squeryl.annotations.Column
import java.sql.Timestamp

object schema extends Schema{

  val users = table[User]("users")

}

case class User(
  @Column("id") override val id: Long = -1,
  @Column("login") login: String,
  @Column("email") email: String,
  @Column("foo_bar") fooBar: Int,
  @Column("foo") bar: Array[Byte]
//  @Column("created_at") createdAt: Timestamp,
//  @Column("updated_at") updatedAt: Timestamp
) extends KeyedEntity[Long]
