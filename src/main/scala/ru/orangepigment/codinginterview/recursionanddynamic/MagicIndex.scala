package ru.orangepigment.codinginterview.recursionanddynamic

import scala.annotation.tailrec

object MagicIndex {

  def main(args: Array[String]): Unit = {
    val arr = Array(-1, 1, 5, 53, 71, 90, 107)
    println(findMagicIndex(arr)())
  }

  @tailrec
  private def findMagicIndex(array: Array[Int])(lower: Int = 0, upper: Int = array.length - 1): Option[Int] =
    if array.isEmpty then None
    else
      val intervalLength = upper - lower + 1
      if (intervalLength == 1) {
        if array(lower) == lower then Some(lower) else None
      } else {
        val middleIdx = intervalLength / 2
        val middleElem = array(middleIdx)
        if middleElem == middleIdx then Some(middleIdx)
        else if middleElem > middleIdx then findMagicIndex(array)(lower, middleIdx - 1)
        else findMagicIndex(array)(middleIdx + 1, upper)
      }

}
