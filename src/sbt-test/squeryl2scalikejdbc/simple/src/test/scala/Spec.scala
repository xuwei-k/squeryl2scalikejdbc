package squeryl2scalikejdbc.test

import org.squeryl.PrimitiveTypeMode.inTransaction
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeExample
import scalikejdbc._
import scalikejdbc.SQLInterpolation._
import models._User

class Spec extends Specification with BeforeExample{
  sequential
  diffs(show=false)

  override def before{
    Main.setup()
    inTransaction{
      schema.create
    }
  }

  "test" should {
    "test" in{
      val user1 = inTransaction{
        schema.users.insert(
          User(login = "foo", email = "bar@example.com")
        )
      }

      1 must_== _User.countBy(sqls"1 = 1")
      List(user1) must_== _User.findAllBy(sqls"1 = 1")
    }
  }
}

