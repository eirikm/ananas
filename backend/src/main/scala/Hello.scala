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

  val db: mutable.Map[String, Session] = mutable.Map[String, Session]()

  def initialSession: Session = Session(Score(0,0), List.empty)

  val plan = unfiltered.filter.Planify {
    case req@POST(Path(Seg("pluss" :: Nil))) =>
      val body = Body.string(req)

      val decodedRegnestykkeRequest = decode[RegnestykkeRequest](body)
      decodedRegnestykkeRequest.fold(
        e =>
          BadRequest ~>
            ResponseString("Could not parse json"),
        regnestykkeRequest => {
          val regnestykke = GenererMattestykker.genererRegnestykke(vanskelighetsGrad = 1)

          val newSession = initialSession.copy(regnestykker = List(regnestykke))
          db(regnestykkeRequest.id) = newSession

          Ok ~>
            ResponseString(
              NyttRegnestykkeResponse(
                regnestykkeRequest.id,
                newSession.score,
                None,
                regnestykke).asJson.spaces2)
        }
      )

    case req@POST(Path(Seg("pluss" :: "svar" :: Nil))) =>
      val body = Body.string(req)

      decode[SvarRequest](body).fold(
        error => {
          println(error)
          BadRequest ~>
            ResponseString("Could not parse json")
        },
        svarRequest => {
          val session = db.getOrElse(svarRequest.id, initialSession)

          val rettetRegnestykke: Option[RettetRegnestykke] = for {
            regnestykke <- session.regnestykker.headOption
            fasit = regnestykke.a + regnestykke.b
          } yield {
            RettetRegnestykke(regnestykke, svarRequest.svar, fasit == svarRequest.svar)
          }

          val nyttRegnestykke = GenererMattestykker.genererRegnestykke(vanskelighetsGrad = 1)

          val newSession: Session = Session(
            {
              val poeng = rettetRegnestykke.count(r => r.riktig)
              val score = session.score.current + poeng
              Score(score, List(score, session.score.highscore).max)
            },
            nyttRegnestykke :: session.regnestykker)


          db(svarRequest.id) = newSession

          val svarResponse = NyttRegnestykkeResponse(
            svarRequest.id,
            newSession.score,
            rettetRegnestykke,
            nyttRegnestykke
          )

          println(svarResponse)

          Ok ~>
            ResponseString(svarResponse.asJson.spaces2)
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

  implicit val scoreEncoder: Encoder[Score] = deriveEncoder
  implicit val regnestykkeEncoder: Encoder[Regnestykke] = deriveEncoder
  implicit val forrigeRegnestykkeEncoder: Encoder[RettetRegnestykke] = deriveEncoder
  implicit val svarResponseEncoder: Encoder[NyttRegnestykkeResponse] = deriveEncoder
}

case class Session(score: Score, regnestykker: List[Regnestykke])

case class Score(current: Int, highscore: Int)


case class RegnestykkeRequest(id: String)

case class Regnestykke(a: Int, op: String, b: Int)

case class SvarRequest(id: String, svar: Int)

case class NyttRegnestykkeResponse(id: String,
                                   score: Score,
                                   forrigeRegnestykke: Option[RettetRegnestykke],
                                   nyttRegnestykke: Regnestykke)

case class RettetRegnestykke(regnestykke: Regnestykke,
                             svarFraBruker: Int,
                             riktig: Boolean)