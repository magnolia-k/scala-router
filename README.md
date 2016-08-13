# router

simple routing module inspired by tokuhirom's Router::Boom and moznion's Router-Boost

# usage

  import router._

  val r = new Router()
  r.add("""/""", "dispatch_root")
  r.add("""/:user""", "dispatch_user")

  r.matchRoutes("/")  // -> returns tuple ("dispatch_root", Map())
  r.matchRoutes("/magnolia")  // -> returns tuple ("dispatch_user", Map("user" -> "magnolia"))
