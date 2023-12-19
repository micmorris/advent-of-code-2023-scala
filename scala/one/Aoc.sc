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

  def run1(input: Stream[String]): Int = {
    input
      .map(line =>
          line.find(_.isDigit)
            .get
            .toString
            .concat(line.findLast(_.isDigit).get.toString)
            .toInt
      ).sum
  }

  def run2(input: Stream[String]): Int = {
    val lines = input.toList
      .map(
        _.split("")
          .map(_.toInt)
      )
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
