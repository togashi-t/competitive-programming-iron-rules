package chapter

import scala.io.StdIn

object Second extends App {

  // 一次元の累積和(1)
  // 特定期間の累積出席者数を算出
  def howManyGuests = {
    val Array(dayCount, dayLineCount) = StdIn.readLine.split(" ").map(_.toInt)
    val numberArray = StdIn.readLine.split(" ").map(_.toInt)
    val dayPairs = for (_ <- 1 to dayLineCount) yield {
      val Array(a, b) = StdIn.readLine().split(" ").map(_.toInt)
      (a, b)
    }
    // 累積和を算出。生成されるArrayの先頭は0になる
    val cumulativeNumberArray = numberArray.scanLeft(0)(_ + _)
    val result = dayPairs.map { case (startDay, endDay) =>
      // 終了日まで累積和から開始日前日までの累積和を差し引く
      cumulativeNumberArray(endDay) - cumulativeNumberArray(startDay)
    }
    result
  }


  // 一次元の累積和(2)
  // 各日の出席者数を算出。このように差分を計算した後に累積和をとるテクニックは競技プログラミングにおいては「いもす法」と呼ばれている。
  def eventAttendance = {
    val dayCount = StdIn.readInt()
    val attendeeCount = StdIn.readInt()
    val dayPairs = for (_ <- 1 to attendeeCount) yield {
      val Array(a, b) = StdIn.readLine().split(" ").map(_.toInt)
      (a, b)
    }

    // 前日比参加者数増減のVector。indexの番号と何日目の数値が一致していた方がわかりやすいので、0日目を設けている(dayCount + 1 の部分)
    val diffFromPrevDayVector = dayPairs.foldLeft(Vector.fill(dayCount + 1)(0)) { case (tmpVector, (startDay, endDay)) =>
      // 出席開始日の前日差人数を従前+1する
      val tmpVector_ = tmpVector.updated(startDay, tmpVector(startDay) + 1)
      // 出席終了翌日の前日差人数を-1する。ただし、出席終了日が最終日である場合この処理は不要（もし行うとVectorの範囲外へのアクセスによりエラーになる）。
      if (endDay == dayCount) tmpVector_ else tmpVector_.updated(endDay + 1, tmpVector(endDay + 1) - 1)
    }
    // それぞれの日の参加者数のVector。累積和を算出することで求める。
    diffFromPrevDayVector
      .tail // 0日目を除外したいので
      .scanLeft(0)(_ + _)
      .tail // scanLeftの使用により先頭に0が追加されるので
  }


}

