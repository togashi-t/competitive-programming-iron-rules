package chapter

import scala.io.StdIn
import scala.util.chaining.scalaUtilChainingOps

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


  // 二次元の累積和(1)
  def twoDimensionalSum = {
    // 標準入力から必要な情報を取得
    val List(height, width) = StdIn.readLine.split(" ").map(_.toInt).toList
    val matrix = (1 to height).toVector.map { _ =>
      StdIn.readLine().split(" ").map(_.toInt).toVector
    }
    val questionCount = StdIn.readInt()
    val questionNumbersList = (1 to questionCount).toList.map { _ =>
      StdIn.readLine().split(" ").map(_.toInt).toList
    }

    // 二次元の累積和の算出。元のmatrixから行、列がそれぞれ1つ増える。最初の行と列の全要素の値は0となる。
    val cumulativeMatrix = {
      // まずは横方向の累積和を求める
      val horizontalCumulativeMatrix = matrix.map { rowNumbers =>
        rowNumbers.scanLeft(0)(_ + _) // 先頭に0が追加されて、元よりも長さが1増える
      }
      // 横方向の累積和を求めたmatrixを基にして縦方向の累積和を求めることで、完成形の累積和のmatrixを求める
      // 最初の行に0を追加
      val extendedMatrix = Vector.fill(1, width + 1)(0) ++ horizontalCumulativeMatrix
      // 2行目以降の行から順番に、各要素に対して1行上の同一列の値を加算する
      (1 to height).foldLeft(extendedMatrix) { case (tmpMatrix, rowIndex) =>
        tmpMatrix.updated(
          rowIndex,
          (0 to width).map(colIndex => tmpMatrix(rowIndex - 1)(colIndex) + tmpMatrix(rowIndex)(colIndex)).toVector
        )
      }
    }

    // 問題への回答
    questionNumbersList.map { case (startRowIndex :: startColIndex :: endRowIndex :: endColIndex :: Nil) =>
      cumulativeMatrix(endRowIndex)(endColIndex) + cumulativeMatrix(startRowIndex - 1)(startColIndex - 1) - cumulativeMatrix(startRowIndex - 1)(endColIndex) - cumulativeMatrix(endRowIndex)(startColIndex - 1)
    }
  }


  // 二次元の累積和(2)
  def winterInAlgoKingdom = {
    // 標準入力から必要な情報を取得
    val List(height, width, dayCount) = StdIn.readLine.split(" ").map(_.toInt).toList
    val snowAreaNumbersList = (1 to dayCount).toList.map { _ =>
      StdIn.readLine().split(" ").map(_.toInt).toList
    }

    // 積雪の前日差を記録するmatrixを作成
    val diffMatrix = {
      // 素材として使用する、全要素が0のmatrix。
      // 要素へのアクセス時にIndexOutOfBoundsExceptionが発生しないようサイズを+1している。余分な行が最下段に、余分な列が最右端に一つ増えるイメージ。
      val initDiffMatrix = Vector.fill(height + 1, width + 1)(0)
      // 入力の数値に従い前日差を記録するmatrixに反映
      snowAreaNumbersList.foldLeft(initDiffMatrix) { case (tmpMatrix, (startRowNum :: startColNum :: endRowNum :: endColNum :: Nil)) =>
        updateElement(tmpMatrix, startRowNum - 1, startColNum - 1, 1) // 対象範囲左上端の要素を+1
          .pipe(x => updateElement(x, endRowNum, endColNum, 1)) // 対象範囲右下端のひとつ右下外を+1
          .pipe(x => updateElement(x, endRowNum, startColNum - 1, -1)) // 対象範囲左下端のひとつ下外を-1
          .pipe(x => updateElement(x, startRowNum - 1, endColNum, -1)) // 対象範囲右上端のひとつ右外を-1
      }
    }

    // 前日差記録のmatrixの累積和を求める
    val cumulativeDiffMatrix = {
      // まずは横方向の累積和を求める
      val horizontalCumulativeMatrix = diffMatrix.map { rowNumbers =>
        rowNumbers.scanLeft(0)(_ + _).tail // 先頭に追加される0を除外するためtailを使用
      }
      // 横方向の累積和を求めたmatrixを基にして縦方向の累積和を求めることで、完成形の累積和のmatrixを求める
      // 2行目以降の行から順番に、各要素に対して1行上の同一列の値を加算する
      (1 until height).foldLeft(horizontalCumulativeMatrix) { case (tmpMatrix, rowIndex) =>
        tmpMatrix.updated(
          rowIndex,
          (0 to width).map(colIndex => tmpMatrix(rowIndex - 1)(colIndex) + tmpMatrix(rowIndex)(colIndex)).toVector
        )
      }
    }

    // matrixは処理の都合上、余分に行と列を1つずつ設けているので、それらをそれぞれ削る。
    cumulativeDiffMatrix.slice(0, height).map(row => row.slice(0, width))
  }

  // 2次元のVectorの要素の値を更新するための関数を定義する。更新が割と手間なので。
  private def updateElement(matrix: Vector[Vector[Int]], rowIndex: Int, colIndex: Int, delta: Int): Vector[Vector[Int]] = {
    val oldValue = matrix(rowIndex)(colIndex)
    val newValue = oldValue + delta
    matrix.updated(rowIndex, matrix(rowIndex).updated(colIndex, newValue))
  }




}

