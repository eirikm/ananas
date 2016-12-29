import unfiltered.request._
import unfiltered.response._

import scala.util.Random
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._


object Hello extends App {
  import JsonCodecs._

  val plan = unfiltered.filter.Planify {
//    case Path(Seg(Nil))             => ResponseString("Hello")
    case Path(Seg("pluss" :: Nil))  =>
      ResponseString(GenererMatteStykker.genererRegnestykke(vanskelighetsGrad = 1).asJson.spaces2)
  }

  val port: Int = Option(System.getenv("ANANAS_PORT")).map(_.toInt).getOrElse(1337)

  unfiltered.jetty.Server
    .http(port)
    .resources(this.getClass.getResource("/"))
    .plan(plan)
    .run()
}

object GenererMatteStykker extends App {
  def genererRegnestykke(vanskelighetsGrad: Int) = {
    val a = Random.nextInt(5)
    val b = Random.nextInt(4)

    Regnestykke(a, b, "+")
  }

  val regnestykke = genererRegnestykke(0)

  import JsonCodecs._
  println(regnestykke.asJson)
}

object JsonCodecs {
  implicit val regnestykkeEncoder: Encoder[Regnestykke] = deriveEncoder
}

case class Regnestykke(a: Int, b: Int, op: String)