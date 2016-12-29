import unfiltered.request._
import unfiltered.response._

object Hello extends App {
  val plan = unfiltered.filter.Planify {
    case Path(Seg(Nil)) => ResponseString("Hello")
  }

  unfiltered.jetty.Server.local(1337).plan(plan).run()
}