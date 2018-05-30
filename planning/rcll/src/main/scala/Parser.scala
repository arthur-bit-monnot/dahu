package dahu.planning.rcll

import fastparse._

object Common {
  sealed trait Tag[X]
  sealed trait RingColorTag
  sealed trait BaseColorTag
  sealed trait CapColorTag
  sealed trait StationTag
  sealed trait StationTypeTag
  type RingColor = String with RingColorTag
  type BaseColor = String with BaseColorTag
  type CapColor = String with CapColorTag
  type Station = String with StationTag
  type StationType = String with StationTypeTag
  type Gate = String

  sealed trait IO
  case object INPUT extends IO
  case object OUTPUT extends IO
  val Start = "START".asInstanceOf[Station]
}
import Common._

case class Order(id: Int,
                 baseColor: BaseColor,
                 ringColors: List[RingColor],
                 capColor: CapColor,
                 timing: Timing,
                 gate: Gate) {
  def complexity = ringColors.size
}

case class TravelTime(ss: Station, sio: IO, es: Station, eio: IO, duration: Double) {

  def durationString = f"$duration%1.3f".replace(',', '.') // probably depends on locale

  def format: String = s"(= (path-length $ss $sio $es $eio) $durationString)"
}

case class Game(orders: Seq[Order]) {
  override def toString: Gate = orders.mkString("\n")
}

case class Problem(order: Order, numRobots: Int, travelTimes: Seq[TravelTime]) {

  val robots = (1 to numRobots).map(i => s"R-$i")

  val stations = ("C-BS" :: "C-CS1" :: "C-CS2" :: "C-DS" :: "C-RS1" :: "C-RS2" :: Nil)
    .asInstanceOf[List[Station]]

  def definedStation(str: String) =
    stations.contains(str.asInstanceOf[Station]) || str == Common.Start

  def typeOf(s: Station): StationType =
    (
      if(s.contains("RS")) "RS"
      else if(s.contains("BS")) "BS"
      else if(s.contains("DS")) "DS"
      else if(s.contains("CS")) "CS"
      else ???
    ).asInstanceOf[StationType]
  def ringStations = stations.filter(typeOf(_) == "RS")
  def capStations = stations.filter(typeOf(_) == "CS")

  def complexity: Int = order.complexity

  val maxActions: Map[String, Int] = Map(
    "bs-dispense" -> 1,
    "cs-mount-cap" -> 1,
    "cs-retrieve-cap" -> 1,
    "enter-field" -> robots.size,
    "move-wp-get" -> 3,
    "move-wp-put-at-input" -> 3,
    "prepare-bs" -> 1,
    "prepare-cs" -> 2, // mount and retrieve
    "prepare-ds" -> 1,
    "prepare-rs" -> order.complexity,
    "rs-mount-ring1" -> (if(complexity >= 1) 1 else 0),
    "wp-discard" -> 1,
    "wp-get" -> 3,
    "wp-get-shelf" -> 1,
    "wp-put" -> 3,
    "fulfill-order-c0" -> (if(complexity == 0) 1 else 0),
    "fulfill-order-c1" -> (if(complexity == 1) 1 else 0),
  )

  def orderInit: String = order match {
    case Order(_, baseColor, Nil, capColor, _, gate) =>
      s"""
         (order-complexity o1 c0)
         (order-base-color o1 $baseColor)
         (order-cap-color o1 $capColor)
         (order-gate o1 $gate)
      """.stripMargin
    case Order(_, baseColor, Seq(ringColor1), capColor, _, gate) =>
      s"""
         ; C1 order
	       (order-complexity o1 c1)
	       (order-base-color o1 $baseColor)
	       (order-ring1-color o1 $ringColor1)
	       (order-cap-color o1 $capColor)
	       (order-gate o1 $gate)
       """.stripMargin
  }

  def asPddl =
    s"""
    (define (problem rcll-production-durative-prob1)
	   (:domain rcll-production-durative)

	(:objects
		${robots.mkString(" ")} - robot
		; If adding R-2 and R-3, also add robot-waiting facts below
		o1 - order
		wp1 - workpiece
		cg1 cg2 cg3 cb1 cb2 cb3 - cap-carrier
		${stations.mkString(" ")} - mps
		CYAN - team-color
	)

	(:init
   ${stations.map(s => s"(mps-type $s ${typeOf(s)})").mkString("\n")}
	 (location-free START INPUT)
   ${stations
         .flatMap(s => (s, "INPUT") :: (s, "OUTPUT") :: Nil)
         .map { case (s, d) => s"(location-free $s $d)" }
         .mkString("\n")}

	 (cs-can-perform C-CS1 CS_RETRIEVE)
	 (cs-can-perform C-CS2 CS_RETRIEVE)
	 (cs-free C-CS1)
	 (cs-free C-CS2)
	 ; Additional base number handling static predicates
	 (rs-sub THREE TWO ONE)
	 (rs-sub THREE ONE TWO)
	 (rs-sub THREE ZERO THREE)
	 (rs-sub TWO TWO ZERO)
	 (rs-sub TWO ONE ONE)
	 (rs-sub TWO ZERO TWO)
	 (rs-sub ONE ONE ZERO)
	 (rs-sub ONE ZERO ONE)
	 (rs-sub ZERO ZERO ZERO)
	 (rs-inc ZERO ONE)
	 (rs-inc ONE TWO)
	 (rs-inc TWO THREE)
	 (rs-filled-with C-RS1 ZERO)
	 (rs-filled-with C-RS2 ZERO)
	 (wp-base-color wp1 BASE_NONE)
	 (wp-cap-color wp1 CAP_NONE)
	 (wp-ring1-color wp1 RING_NONE)
	 (wp-ring2-color wp1 RING_NONE)
	 (wp-ring3-color wp1 RING_NONE)
	 (wp-unused wp1)
   ${robots.map(r => s"(robot-waiting $r)").mkString("\n")}
	 ;(robot-waiting R-1)
	 ;(robot-waiting R-2)
	 ;(robot-waiting R-3)

	 ${stations.map(s => s"(mps-state $s IDLE)").mkString("\n")}
   ;(mps-state C-BS IDLE)
 	 ;(mps-state C-CS1 IDLE)
 	 ;(mps-state C-CS2 IDLE)
	 ;(mps-state C-DS IDLE)
 	 ;(mps-state C-RS1 IDLE)
 	 ;(mps-state C-RS2 IDLE)

	 (wp-cap-color cg1 CAP_GREY)
	 (wp-cap-color cg2 CAP_GREY)
	 (wp-cap-color cg3 CAP_GREY)
	 (wp-on-shelf cg1 C-CS1 LEFT)
	 (wp-on-shelf cg2 C-CS1 MIDDLE)
	 (wp-on-shelf cg3 C-CS1 RIGHT)

	 (wp-cap-color cb1 CAP_BLACK)
	 (wp-cap-color cb2 CAP_BLACK)
	 (wp-cap-color cb3 CAP_BLACK)
	 (wp-on-shelf cb1 C-CS2 LEFT)
	 (wp-on-shelf cb2 C-CS2 MIDDLE)
	 (wp-on-shelf cb3 C-CS2 RIGHT)

	 (rs-ring-spec C-RS1 RING_GREEN ZERO)
	 (rs-ring-spec C-RS1 RING_YELLOW ZERO)
	 (rs-ring-spec C-RS2 RING_BLUE ONE)
	 (rs-ring-spec C-RS2 RING_ORANGE TWO)

   $orderInit

	 ; C0 order
	 ;(order-complexity o1 c0)
	 ;(order-base-color o1 BASE_BLACK)
	 ;(order-cap-color o1 CAP_GREY)
	 ;(order-gate o1 GATE-1)

	 ; These values are for the static default world
   ${travelTimes
         .filter(tt => definedStation(tt.es) && definedStation(tt.ss))
         .map(_.format)
         .mkString("\n")}

	)

	(:goal (order-fulfilled o1))
)
    """.stripMargin
}

case class Timing(placeholder: Int = 0)

object Parser {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhileIn(" \n").rep)
  }
  import fastparse.noApi._
  import White._

  val word = P(CharsWhileIn(('A' to 'Z') ++ ('a' to 'z') ++ Seq('-', '_')))

  val int = P(CharsWhileIn("0123456789")).!.map(_.toInt)
  val gate: P[Gate] =
    P("D1".!.map(_ => "GATE-1") | "D2".!.map(_ => "GATE-2") | "D3".!.map(_ => "GATE-3"))
      .map(_.asInstanceOf[Gate])

  val complexity = P("C0" | "C1" | "C2" | "C3")
  val baseColor = P("BASE_" ~ CharsWhileIn('A' to 'Z')).!.map(_.asInstanceOf[BaseColor])
  val capColor = P("CAP_" ~ CharsWhileIn('A' to 'Z')).!.map(_.asInstanceOf[CapColor])
  val ringColor = P("RING_" ~ CharsWhileIn('A' to 'Z')).!.map(_.asInstanceOf[RingColor])
  val ringStation = P(word ~ int).!

  val timing =
    P("from" ~ int ~ ":" ~ int ~ "to" ~ int ~ ":" ~ int ~ "(@" ~ int ~ ":" ~ int ~ "~" ~ int ~ "s)")
      .map(_ => Timing())

  val order = P(
    ("Order" ~/ int ~/ ":" ~/ complexity ~/ "(" ~ baseColor ~ "|" ~ ringColor
      .rep(min = 0, max = 3) ~ "|" ~ capColor ~ ")" ~ timing ~ gate).map {
      case (id, bc, rcs, cc, timing, dest) => Order(id, bc, rcs.toList, cc, timing, dest)
    })
  val additionalBase = P("Ring color" ~/ ringColor ~/ "requires" ~/ int ~/ "additional bases").!

  val ringStationColors = P("RS" ~/ ringStation ~/ "has colors" ~ "(" ~ ringColor.rep ~ ")")

  val game = (order.rep(1) ~ additionalBase.rep ~ ringStationColors.rep ~ End).map {
    case (orders, _, _) => Game(orders)
  }

  val alphaNumSeq = P(CharsWhileIn(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')))
  val travelTimeStation: P[Station] = P(alphaNumSeq ~/ "-" ~/ alphaNumSeq).!.map {
    case "C-ins" => Common.Start
    case x       => x.asInstanceOf[Station]
  }
  val float = P(int ~ "." ~ int).!.map(_.toDouble)
  val io = P("I".!.map(_ => INPUT) | "in".!.map(_ => INPUT) | "O".!.map(_ => OUTPUT))
  val travelTime =
    P(travelTimeStation ~ "-" ~ io ~ ";" ~ travelTimeStation ~ "-" ~ io ~ ";" ~ float).map {
      case (ss, sio, es, eio, t) => TravelTime(ss, sio, es, eio, t)
    }

  val travelTimes = travelTime.rep ~ End

}
