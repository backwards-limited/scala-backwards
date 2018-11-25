package com.backwards.config

import io.lemonlabs.uri.Uri
import org.scalatest.{Assertion, MustMatchers, WordSpec}

class ConfigSpec extends WordSpec with MustMatchers {
  def checkConfig(myconfig: MyConfig): Assertion = {
    myconfig.bootstrap mustBe BootstrapConfig(Seq(Uri.parse("127.0.0.1:9092")))

    myconfig.properties("property.1") mustEqual "1"
    myconfig.properties("property.2") mustEqual "true"
  }

  "Config using Object" should {
    import com.backwards.config.Config._

    "read be read including properties from resource configuration" in {
      checkConfig(load[MyConfig]("myconfig"))
    }
  }

  "Config using Trait" should {
    "read be read including properties from resource configuration" in new Config {
      checkConfig(load[MyConfig]("myconfig"))
    }
  }
}