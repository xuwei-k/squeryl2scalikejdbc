package squeryl2scalikejdbc.test

import java.sql.DriverManager
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters.H2Adapter
import org.squeryl.{Session, SessionFactory}
import scalikejdbc.JDBCSettings

object Main{

  val defaultSettings = JDBCSettings(
    url = "jdbc:h2:mem:scalikejdbc",
    user = "hoge",
    password = "",
    driverName = "org.h2.Driver"
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
    adapter: DatabaseAdapter = new H2Adapter()
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


