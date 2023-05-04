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

}
