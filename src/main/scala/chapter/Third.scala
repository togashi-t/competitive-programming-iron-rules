package chapter

import scala.annotation.tailrec
import scala.io.StdIn

object Third {

  // 二分探索
  def binarySearch1 = {
    val List(initLength, target) = StdIn.readLine().split(" ").map(_.toInt).toList
    val numbers = StdIn.readLine().split(" ").map(_.toInt).toVector

    @tailrec
    def searchIndexFunc(numbers: Vector[Int], target: Int, minIndex: Int, maxIndex: Int): Int = {
      val middleIndex = (maxIndex + minIndex) / 2
      val selectedNumber = numbers(middleIndex)
      if (selectedNumber == target) {
        middleIndex
      } else if (target < selectedNumber) {
        searchIndexFunc(numbers, target, minIndex, middleIndex - 1)
      } else {
        searchIndexFunc(numbers, target, middleIndex + 1, maxIndex)
      }
    }

    searchIndexFunc(numbers, target, 0, initLength - 1) + 1 // indexは0スタートなので1を加算して何番目という数値と合わせる
  }


  // 答えで二分探索
  def printer = {
    val List(count, targetPrintNumber) = StdIn.readLine().split(" ").map(_.toLong).toList
    val intervals = StdIn.readLine().split(" ").map(_.toLong).toList

    // 経過時間から印刷枚数を返す
    def printNumberFromMinutes(minutes: Long): Long = intervals.map(minutes / _).sum

    // 経過時間での印刷枚数が目標印刷枚数を達成しているかを返す
    def isGreaterThanTargetPrintNumber(minutes: Long): Boolean = printNumberFromMinutes(minutes) >= targetPrintNumber

    @tailrec
    def searchMinutesFunc(minMinutes: Long, maxMinutes: Long): Long = {
      if (maxMinutes == minMinutes) { // 調査対象時間の範囲が尽きた場合
        minMinutes
      } else {
        val middleMinutes = (minMinutes + maxMinutes) / 2
        if (isGreaterThanTargetPrintNumber(middleMinutes)) { // 印刷枚数が目標以上の場合
          searchMinutesFunc(minMinutes, middleMinutes)
        } else { // 印刷枚数が目標を下回る場合。すなわちmiddleMinutesでは時間不足の場合
          searchMinutesFunc(middleMinutes + 1, maxMinutes)
        }
      }
    }

    searchMinutesFunc(1, 1000000000)
  }

}
