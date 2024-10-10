package ru.orangepigment.codinginterview.recursionanddynamic

object ChildOnStairs {

  def main(args: Array[String]): Unit = {
    println()
    println(waysCount(args(0).toInt)())
  }

  private def waysCount(n: Int)(memo: Array[Option[Int]] = Array.fill(n)(Option.empty[Int])): Int =
    if n == 0 then 1
    else
      val memoIdx = n - 1
      memo(memoIdx).fold {
        val ways =
          List(1, 2, 3).map(
            i => if n - i >= 0 then
              waysCount(n - i)(memo)
            else 0
          ).sum
        memo(memoIdx) = Option(ways)
        ways
      }(identity)

}
