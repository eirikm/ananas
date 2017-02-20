import unfiltered.request._
import unfiltered.response._

import scala.util.Random
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser.decode

import scala.collection.mutable

object Hello extends App {

  import JsonCodecs._

  val db = mutable.HashMap()

  val plan = unfiltered.filter.Planify {
    case OPTIONS(Path(Seg("pluss" :: Nil))) =>
      Ok ~>
        ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("POST, GET")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With"))

    case req @ POST(Path(Seg("pluss" :: Nil))) =>
      val body = Body.string(req)

      val decodedRegnestykkeRequest = decode[RegnestykkeRequest](body)
      decodedRegnestykkeRequest.fold(
        e =>
          BadRequest ~>
            ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
            ResponseString("Could not parse json"),
        regnestykkeRequest => {
          
          Ok ~>
            ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
            ResponseString(GenererMattestykker.genererRegnestykke(vanskelighetsGrad = 1).asJson.spaces2)
        }
      )

    case OPTIONS(Path(Seg("pluss" :: "svar" :: Nil))) =>
      Ok ~>
        ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("POST, GET")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With"))

    case req @ POST(Path(Seg("pluss" :: "svar" :: Nil))) =>
      val body = Body.string(req)

      decode[SvarRequest](body).fold(
        error => {
          println(error)
          BadRequest ~>
            ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
            ResponseString("Could not parse json")
        },
        svarRequest => {
          println(svarRequest)
          Ok ~>
            ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
            ResponseString(GenererMattestykker.genererRegnestykke(vanskelighetsGrad = 1).asJson.spaces2)
        }
      )
  }

  val port: Int = Option(System.getenv("ANANAS_PORT")).map(_.toInt).getOrElse(1337)

  unfiltered.jetty.Server
    .http(port)
    .resources(this.getClass.getResource("/elm-dist/"))
    .plan(plan)
    .run()
}

object GenererMattestykker extends App {
  def genererRegnestykke(vanskelighetsGrad: Int) = {
    val a = Random.nextInt(5)
    val b = Random.nextInt(4)

    Regnestykke(a, "+", b)
  }

  val regnestykke = genererRegnestykke(0)

  import JsonCodecs._

  println(regnestykke.asJson)
}

object JsonCodecs {
  implicit val regnestykkeRequestDecoder: Decoder[RegnestykkeRequest] = deriveDecoder
  implicit val svarRequestDecoder: Decoder[SvarRequest] = deriveDecoder

  implicit val regnestykkeEncoder: Encoder[Regnestykke] = deriveEncoder
}

case class RegnestykkeRequest(id: String)
case class Regnestykke(a: Int, op: String, b: Int)
case class SvarRequest(id: String, svar: Int)
