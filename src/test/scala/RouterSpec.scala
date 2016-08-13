import org.specs2.mutable.Specification

import router._

class RouterSpec extends Specification {

  "basic test" >> {
    val router = new Router()
    
    router.add("""/""", "dispatch_root")
    router.add("""/entrylist""", "dispatch_entrylist")
    router.add("""/:user""", "dispatch_user")
    router.add("""/:user/{year}""", "dispatch_year")
    router.add("""/:user/{year}/{month:\d+}""", "dispatch_month")
    router.add("""/download/*""", "dispatch_download")

    "dispatch_root" >> {
      val m = router.matchRoutes("/")
      m must beSome { v:(String, Map[String, String]) =>
        v._1 === "dispatch_root"
        v._2.size === 0
      }
    }

    "dispatch_entrylist" >> {
      val m = router.matchRoutes("/entrylist")
      m must beSome { v:(String, Map[String, String]) =>
        v._1 === "dispatch_entrylist"
        v._2.size === 0
      }
    }

    "dispatch_user" >> {
      val m = router.matchRoutes("/gfx")
      m must beSome { v:(String, Map[String, String]) =>
        v._1 === "dispatch_user"
        v._2 must havePair( "user" -> "gfx" )
      }
    }

    "dispatch_month" >> {
      val m = router.matchRoutes("/gfx/2013/12")
      m must beSome { v:(String, Map[String, String]) =>
        v._1 === "dispatch_month"
        v._2 must havePairs( "user" -> "gfx", "year" -> "2013", "month" -> "12" )
      }
    }

    "dispatch_download" >> { 
      val m = router.matchRoutes("/download/foo/bar/baz.zip")
      m must beSome { v:(String, Map[String, String]) =>
        v._1 === "dispatch_download"
        v._2 must havePair( "*" -> "foo/bar/baz.zip" )
      }
    }

    "None" >> {
      val m = router.matchRoutes("/gfx/2013/gorou")
      m must beNone
    }
  }

  "capture test" >> {
    "valid" >> {
      val r = new Router()
      r.add("""{foo:(?:.)}""", "valid_root") must not(throwA[java.lang.Exception])
    }

    "invalid" >> {
      val r = new Router()
      r.add("""/{foo:(.)}""", "invalid_root") must throwA[java.lang.Exception]
    }
  }
}
