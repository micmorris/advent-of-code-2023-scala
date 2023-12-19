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
    val schematic = input.map(_.trim
      .split("")
      .map(_.charAt(0))
      .toList
    ).toList
    val validNumbers = schematic.zipWithIndex.flatMap {
      case (row, columnIndex) => row.zipWithIndex map {
        case (item, rowIndex) if !item.isDigit =>
          0
        case (item, rowIndex) if rowIndex > 0 && row(rowIndex - 1).isDigit =>
          0
        case (item, rowIndex) =>
          val numberAsString = collectNumber("", row.slice(rowIndex, row.size))
//          println(s"Number: $numberAsString")
          if (hasAdjacentSymbol(rowIndex, columnIndex, numberAsString.size, schematic)) {
            numberAsString.toInt
          } else {
            0
          }
      }
    }
//    println("Valid numbers:")
//    validNumbers.filterNot(_ == 0).foreach(println)
    validNumbers.sum
  }

  @tailrec
  def collectNumber(accumulator: String, remainingRow: List[Char]): String = {
    remainingRow match {
      case row if row.isEmpty => accumulator
      case char :: tail if !char.isDigit => accumulator
      case char :: tail => collectNumber(accumulator + char, tail)
    }
  }

  def hasAdjacentSymbol(rowIndex: Int, columnIndex: Int, numberLength: Int, schematic: List[List[Char]]): Boolean = {
    ((rowIndex - 1) to  (rowIndex + numberLength))
      .filter(_ >= 0)
      .filter(_ < schematic(0).size)
      .flatMap(currRow =>
        ((columnIndex - 1) to  (columnIndex + 1))
          .filter(_ >= 0)
          .filter(_ < schematic.size)
          .map(currCol => (currCol, currRow))
      )
      .map(coord => {
        val check = schematic(coord._1)(coord._2)
//        println(s"Check: $check Row: ${coord._2} Col: ${coord._1}")
        check
      })
      .exists(char => !char.isDigit && char != '.')
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
