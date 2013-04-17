package squeryl2scalikejdbc.test

import java.sql.DriverManager
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.{Session, SessionFactory}
import scalikejdbc.JDBCSettings

object Main{

  val defaultSettings = JDBCSettings(
    url = "jdbc:postgresql://localhost:5432/squeryl2scalikejdbc",
    user = "postgres",
    password = "",
    driverName = "org.postgresql.Driver"
  )

  def setup(settings: scalikejdbc.JDBCSettings = defaultSettings){
    Class.forName(settings.driverName)
    setupScalikejdbc(settings)
    setupSqueryl(settings)
  }

  def setupScalikejdbc(settings: JDBCSettings){
    import settings._
    scalikejdbc.ConnectionPool.singleton(url, user, password)
  }

  def setupSqueryl(
    settings: scalikejdbc.JDBCSettings,
    adapter: DatabaseAdapter = new PostgreSqlAdapter()
  ){
    import settings._
    SessionFactory.concreteFactory = Some(() => {
      val connection = DriverManager.getConnection(url, user, password)
      val session = Session.create(connection, adapter)
      session.setLogger(println(_))
      session
    })
  }

}


