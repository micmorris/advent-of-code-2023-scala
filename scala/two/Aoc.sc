import $ivy.`com.lihaoyi::os-lib:0.9.2`
import $ivy.`com.lihaoyi::mainargs:0.4.0`
import mainargs.main
import os._

import java.io.{BufferedReader, InputStreamReader}
import scala.language.postfixOps

val USAGE =
  """Usages:
    |# amm scala/one/Aoc.sc [p1 | p2 | test] [sample | input]
    |  amm scala/one/Aoc.sc p1 scala/one/sample
    |  amm scala/one/Aoc.sc p2 scala/one/input
    |  amm scala/one/Aoc.sc test
    |""".stripMargin

@main
def main(part: String, maybeFilename: Option[String]): Int = {
  (part, maybeFilename) match {
    case ("test", _) =>
      Test.runAllTests
    case ("p1", Some(filename)) =>
      Aoc.run1(Setup.readInput(filename))
    case ("p2", Some(filename)) =>
      Aoc.run2(Setup.readInput(filename))
    case _ =>
      println("Invalid input given.")
      -1
  }
}

object Aoc {
  import GamePieces._

  def run1(input: Stream[String]): Int = {
    val linePattern = "Game (\\d+): (.*)".r
    val games = input.map {
      case linePattern(gameId, gamesText) => parseGame(gamesText, gameId.toInt)
      case _                              => ???
    }

    val possibleGoodGameIds = games.map {
      case game if game.redPulls.exists(_ > 12) => 0
      case game if game.greenPulls.exists(_ > 13) => 0
      case game if game.bluePulls.exists(_ > 14) => 0
      case game => game.id
    }

    println("Game outcomes:")
    possibleGoodGameIds.filter(_ > 0).foreach(println)
    possibleGoodGameIds.sum
  }

  def parseGame(gamesText: String, id: Int): Game = {
    val redPattern = "(\\d+) red".r
    val bluePattern = "(\\d+) blue".r
    val greenPattern = "(\\d+) green".r
    val pullsMap = gamesText
      .split("; ")
      .flatMap(pull =>
        pull.split(", ") map {
          case redPattern(num)   => "red" -> num.toInt
          case bluePattern(num)  => "blue" -> num.toInt
          case greenPattern(num) => "green" -> num.toInt
        }
      )
      .groupBy(_._1)
      .mapValues(_.map(_._2).toList)

    Game(
      id = id,
      redPulls = pullsMap.getOrElse("red", List.empty[Int]),
      greenPulls = pullsMap.getOrElse("green", List.empty[Int]),
      bluePulls = pullsMap.getOrElse("blue", List.empty[Int])
    )
  }

  def run2(input: Stream[String]): Int = {
    ???
  }

}

object Test {

  def runAllTests: Int = {
    testThing
    0
  }

  private def testThing: Unit = {
    assert(true)
  }
}

object GamePieces {
  case class Game(id: Int, redPulls: List[Int], greenPulls: List[Int], bluePulls: List[Int])
}

object Setup {

  def readInput(filename: String): Stream[String] = {
    val fileReader = new BufferedReader(new InputStreamReader(read.inputStream(pwd / RelPath(filename))))
    Stream
      .continually(fileReader.readLine())
      .takeWhile(_ != null)
      .map(_.trim)
      .filter(_.nonEmpty)
  }
}
