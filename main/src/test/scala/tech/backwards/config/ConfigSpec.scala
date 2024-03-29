package tech.backwards.config

import io.lemonlabs.uri.Uri
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigSpec extends AnyWordSpec with Matchers {
  def checkConfig(myconfig: MyConfig): Assertion = {
    myconfig.bootstrap mustBe BootstrapConfig(Seq(Uri.parse("http://127.0.0.1:9092")))

    myconfig.properties("property.1") mustEqual "1"
    myconfig.properties("property.2") mustEqual "true"
  }

  "Config using Object" should {
    import tech.backwards.config.Config._

    "read be read including properties from resource configuration" in {
      checkConfig(load[MyConfig]("application.test.conf", "myconfig"))
    }
  }

  "Config using Trait" should {
    "read be read including properties from resource configuration" in new Config {
      checkConfig(load[MyConfig]("application.test.conf", "myconfig"))
    }
  }

  // TODO - WIP
  "Config from a particular file" should {
    "be read" in new Config {
      // TODO - Fails after upgrading to latest pureconfig
      // println(load[SpecificConfig](Resource getUrl "application.dev1.conf", "specific-config"))
    }

    "and again from a different file" in new Config {
      // TODO - Fails after upgrading to latest pureconfig
      // println(load[SpecificConfig](Resource getUrl "application.dev2.conf", "specific-config"))
    }
  }
}