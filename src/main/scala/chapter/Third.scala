package chapter

import scala.annotation.tailrec
import scala.collection.Searching.Found
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


  // しゃくとり法
  def closePairs = {
    // 使用する値の取得
    val List(count, targetDiff) = StdIn.readLine().split(" ").map(_.toInt).toList
    val numbers = StdIn.readLine().split(" ").map(_.toInt).toVector
    val maxIndex = numbers.length - 1

    // コレクション内から取り出した2つの値の組み合わせの内、その差が指定値以下となる組み合わせの数を数える
    @tailrec
    def go(leftIndex: Int, rightIndex: Int, count: Long): Long = {
      if (leftIndex == maxIndex) { // pairの小さい方の値がコレクションの右端になったら終了
        count
      } else {
        // 組合せの差
        val diff = numbers(rightIndex) - numbers(leftIndex)
        // rightIndexをより大きくする余地があるかの確認
        if ((rightIndex < maxIndex) && diff <= targetDiff) { // rightIndexをより大きくする余地がある場合
          go(leftIndex, rightIndex + 1, count)
        } else { // rightIndexをより大きくする余地がない場合
          val delta = rightIndex - leftIndex + (if (diff <= targetDiff) 0 else -1) // rightIndexが右端にあるとき、場合によってはdiff <= targetDiffとなる
          go(leftIndex + 1, rightIndex, count + delta)
        }
      }
    }

    go(0, 1, 0)
  }


  // 半分全列挙
  def fourBoxes = {
    // 使用する値の取得
    val List(cardCount, target) = StdIn.readLine().split(" ").map(_.toInt).toList
    val cardsList = (for (_ <- 1 to 4) yield {
      StdIn.readLine().split(" ").map(_.toInt).toList
    }).toList


    val cardANumList :: cardBNumList :: cardCNumList :: cardDNumList :: Nil = cardsList
    // カードAとBの全ての組み合わせの足し合わせた数のリストを作成
    val cardABSumList = for {
      cardANum <- cardANumList
      cardBNum <- cardBNumList
    } yield {
      cardANum + cardBNum
    }
    // カードCとDの全ての組み合わせの足し合わせた数のリストを作成。このリスト内の要素は二分探索するので、この時点で昇順に並び替えておく。
    val cardCDSumSortedList = (for {
      cardCNum <- cardCNumList
      cardDNum <- cardDNumList
    } yield {
      cardCNum + cardDNum
    }).sorted

    // cardABSumListとcardCDSumSortedListからそれぞれ1つずつ取り出して加算した結果で、目的の数値と合致するものがあるかを調べる
    val result: Boolean = cardABSumList.exists { abSum =>
      // 目的の数値との差
      val diff = target - abSum
      // 目的の数値との差に合致するものがあるかを調べる
      cardCDSumSortedList.search(diff) match {
        case Found(_) => true
        case _ => false
      }
    }

    if (result) "Yes" else "No"
  }

}
