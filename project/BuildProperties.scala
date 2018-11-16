import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConverters.propertiesAsScalaMap
import scala.util.Try
import sbt._

object BuildProperties {
  lazy val environment: Map[String, String] =
    Try(new FileInputStream(".env")).map { env =>
      val properties = new Properties
      properties load env

      propertiesAsScalaMap(properties).toMap
    } getOrElse Map.empty[String, String]
  
  private lazy val properties = {
    val p = new Properties()
    p.load(new FileInputStream(file("project/build.properties").asFile))
    p
  }

  def apply(property: String): String = properties getProperty property
}