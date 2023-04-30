package chapter

import scala.io.StdIn
import scala.math.pow

object First extends App {

  // 正方形の面積
  def squareArea = {
    val length = StdIn.readInt()
    length * length
  }

  // 全探索part1
  def linerSearch = {
    val Array(count, target) = StdIn.readLine.split(" ").map(_.toInt)
    val numberArray = StdIn.readLine.split(" ").map(_.toInt)
    numberArray.contains(target)
  }

  // 全探索part2
  def twoCards = {
    val Array(_, target) = StdIn.readLine.split(" ").map(_.toInt)
    val redNumberArray = StdIn.readLine.split(" ").map(_.toInt)
    val blueNumberArray = StdIn.readLine.split(" ").map(_.toInt)
    val result = (for {
      redNumber <- redNumberArray
      blueNumber <- blueNumberArray
    } yield {
      redNumber + blueNumber == target
    }).contains(true)
    result
  }

  // 2進法。ある整数を2進数表記に変換。
  def binaryRepresentation = {
    val num = StdIn.readInt()
    val res = (for (i <- 9 to 0 by -1) yield num / pow(2, i).toInt % 2)
      .map(_.toString).mkString("")
    res
  }

  // 3種類のカードの数字の組合せた加算で目的の数値に合致するパターンが何通りあるか
  // 単純に3種類の全カードの数値の組合せを全て網羅しようとすると計算量がO(Nの3乗)と大きくなり過ぎるのでこうする。
  def threeCards = {
    val Array(maxNum, targetNum) = StdIn.readLine.split(" ").map(_.toInt)
    val numRange = 1 to maxNum
    val resultCount = (for {
      firstNum <- numRange
      secondNum <- numRange
      diff = targetNum - firstNum - secondNum // 目的の数値までの差
      if diff > 0 && diff <= maxNum // 目的数値までの差を埋めることは可能なのか
    } yield 1).length
    resultCount
  }


  // ビット全探索。nums中の数値を組み合わせて加算することでtargetの値に一致させることができるか
  def bitFullSearch = {
    val nums = List(1, 2, 4, 7)
    val target = 13
    // 何ビットか
    val n = nums.length
    // 全ての組み合わせの数（2^n 通り）を計算する。1 << n は、2進数で表現された1をnビット左にシフトすることを意味する。
    // 左シフト演算は、右側に0を追加することで数値を2倍する。
    // この操作をn回行うと、数値は 2^n 倍になる。したがって、1 << n は 2^n と等価。
    val totalCombinations = 1 << n

    // nビットでの表現を整数にしたものを全て試す
    (0 until totalCombinations).exists { i =>
      // その整数をビット表現にしたときに各桁が1と0のいずれかであるかを特定し、1の場合のみ該当数値を加算。
      val subsetSum = (0 until n).foldLeft(0) { (sum, j) =>
        // i >> j：整数iのビットをjビット分右にシフト。これによりj番目のビットが一番右（最下位）のビットに移動。
        // (i >> j) & 1：シフトした結果の最下位ビット（j番目の元のビット）が1かどうかを調べるために、ビットごとの論理積（AND）演算子 & を使って、最下位ビット以外のビットをすべて0にする。
        // 最下位ビットが1であれば結果は1になり、最下位ビットが0であれば結果は0になる。
        if (((i >> j) & 1) == 1) sum + nums(j) else sum
      }
      subsetSum == target
    }
  }

}




//object Main {
//  def main(args: Array[String]): Unit = {
//    val input = StdIn.readInt()
//    println(input * input)
//  }
//}
