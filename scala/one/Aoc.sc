import $ivy.`com.lihaoyi::os-lib:0.9.2`
import $ivy.`com.lihaoyi::mainargs:0.4.0`
import mainargs.main
import os._

import java.io.{BufferedReader, InputStreamReader}
import scala.annotation.tailrec
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
      Aoc.run1(Setup.readInput(filename), List.empty.toStream)
    case ("p2", Some(filename)) =>
      Aoc.run2(Setup.readInput(filename))
    case _ =>
      println("Invalid input given.")
      -1
  }
}

object Aoc {

  def run1(input: Stream[String], original: Stream[String]): Int = {
    val inputList = input.toList
    val values = inputList
      .map(line =>
          line.find(_.isDigit)
            .get
            .toString
            .concat(line.findLast(_.isDigit).get.toString)
            .toInt
      )
//    println("Calibrations:")
//    original.zipAll(values, "", 0)
//      .foreach(println)
    values.sum
  }

  def run2(input: Stream[String]): Int = {
    val modifiedInput = input.map(line => findNextNumber("", line)).toList
//    modifiedInput.foreach(println)
    run1(modifiedInput.toStream, input)
  }

  @tailrec
  def findNextNumber(parsedString: String, currString: String): String = {
    currString match {
      case "" => parsedString
      case str if str.startsWith("one") => findNextNumber(parsedString + "1" + str.charAt(0), str.substring(1))
      case str if str.startsWith("two") => findNextNumber(parsedString + "2" + str.charAt(0), str.substring(1))
      case str if str.startsWith("three") => findNextNumber(parsedString + "3" + str.charAt(0), str.substring(1))
      case str if str.startsWith("four") => findNextNumber(parsedString + "4" + str.charAt(0), str.substring(1))
      case str if str.startsWith("five") => findNextNumber(parsedString + "5" + str.charAt(0), str.substring(1))
      case str if str.startsWith("six") => findNextNumber(parsedString + "6" + str.charAt(0), str.substring(1))
      case str if str.startsWith("seven") => findNextNumber(parsedString + "7" + str.charAt(0), str.substring(1))
      case str if str.startsWith("eight") => findNextNumber(parsedString + "8" + str.charAt(0), str.substring(1))
      case str if str.startsWith("nine") => findNextNumber(parsedString + "9" + str.charAt(0), str.substring(1))
      case str => findNextNumber(parsedString + str.charAt(0), str.substring(1))
    }
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
