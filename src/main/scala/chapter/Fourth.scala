package chapter

// 動的計画法
object Fourth {


  def dungeon1 = {
    val scanner = new java.util.Scanner(System.in)
    val count = scanner.nextInt()
    val singleCosts = Vector.fill(count - 1)(scanner.nextInt())
    val doubleCosts = Vector.fill(count - 2)(scanner.nextInt())

    val dp = Array.fill(count + 1)(0) // 部屋番号とindexが一致するよう、要素数を部屋数+1としている。
    // 1番目の部屋に行くコストは0だが、初期値0なので処理不要
    // 2番目の部屋に行くコストを入れる
    dp(2) = singleCosts.head
    // 3番目以降の部屋のコストを順番に求めていく
    for (n <- 3 to count) yield {
      val singleCost = dp(n - 1) + singleCosts(n - 2)
      val doubleCost = dp(n - 2) + doubleCosts(n - 3)
      dp(n) = math.min(singleCost, doubleCost)
    }

    dp(count)
  }


  def dungeon2 = {
    val scanner = new java.util.Scanner(System.in)
    val count = scanner.nextInt()
    val singleCosts = Vector.fill(count - 1)(scanner.nextInt())
    val doubleCosts = Vector.fill(count - 2)(scanner.nextInt())

    val dp = Array.fill(count + 1)(0) // 部屋番号とindexが一致するよう、要素数を部屋数+1としている。
    // 1番目の部屋に行くコストは0だが、初期値0なので処理不要
    // 2番目の部屋に行くコストを入れる
    dp(2) = singleCosts.head
    // 3番目以降の部屋のコストを順番に求めていく
    for (n <- 3 to count) yield {
      val singleCost = dp(n - 1) + singleCosts(n - 2)
      val doubleCost = dp(n - 2) + doubleCosts(n - 3)
      dp(n) = math.min(singleCost, doubleCost)
    }

    // pathRoomListは現時点でわかっている最短経路の部屋の番号のリスト。番号が小さい順。
    @scala.annotation.tailrec
    def go(pathRoomList: List[Int]): List[Int] = {
      // 最も小さい部屋番号
      val headRoom = pathRoomList.head
      if (headRoom == 1) {
        pathRoomList
      } else if (headRoom == 2) {
        1 :: pathRoomList
      } else {
        // 今の最も小さい部屋番号にどこの部屋から来たのかを特定し、その部屋番号をリストに追加する。
        val delta = if (dp(headRoom) - dp(headRoom - 1) == singleCosts(headRoom - 2)) -1 else -2
        go((headRoom + delta) :: pathRoomList)
      }
    }

    go(List(count))
  }


  // 二次元のDP(1)
  def subsetSum = {
    val scanner = new java.util.Scanner(System.in)
    val count = scanner.nextInt()
    val target = scanner.nextInt()
    val numbers = Vector.fill(count)(scanner.nextInt())

    // dpの列(0番から始まってtarget番目まで)は数値を加算した合計値に対応させて使用
    val dp = Array.ofDim[Boolean](count + 1, target + 1)
    // カードを1つも選んでいない状態での値は0になるため
    dp(0)(0) = true

    // カードを順に使用してdpを更新していく
    @scala.annotation.tailrec
    def updateDp(currentRowIndex: Int, prevRowTrueColIndexSet: Set[Int]): Array[Array[Boolean]] = {
      if (currentRowIndex > count) {
        dp
      } else {
        // 加算する数。カードとrowのindexの対応関係は1相違があるので次のようにしている。例、カードのindex=0とrowのindex=1が対応する。
        val delta = numbers(currentRowIndex - 1)
        // どの列のindexをtrueにするか。前の行のindexは無条件でtrue。
        // また、前の行でtrueとなっているindexにdeltaを加算したindexもtrue。ただし、targetよい大きいindexは除外する。Array範囲外への参照により例外が発生することを回避するため。
        val currentRowTrueColIndexSet = prevRowTrueColIndexSet ++ prevRowTrueColIndexSet.map { x: Int => x + delta }.filter { x: Int => x <= target }
        // dpの対象行の更新
        currentRowTrueColIndexSet.foreach { x => dp(currentRowIndex)(x) = true }
        // 次の行を更新対象行に指定
        updateDp(currentRowIndex + 1, currentRowTrueColIndexSet)
      }
    }

    updateDp(1, Set(0))

    if (dp(count)(target)) "Yes" else "No"
  }



  // 二次元のDP(2):ナップザック問題
  def knapsack1 = {
    val scanner = new java.util.Scanner(System.in)
    val count = scanner.nextInt()
    val maxWeight = scanner.nextInt()
    val weightAndValues = (for (_ <- 1 to count) yield {
      val Array(w, v) = Array.fill(2)(scanner.nextInt())
      (w, v.toLong)
    }).toVector

    // 全要素が-1である2次元の配列を作成。
    // 行は何番目の品物であるかを、列は重さの合計を表す。
    // 各マスの値は、価値の合計値を表す。
    val dp = Array.tabulate(count + 1, maxWeight + 1) { (_, _) => - 1L}
    // 何も選んでいない場合は重さと価値は0なので
    dp(0)(0) = 0

    // dpの各行を更新していくための関数
    @scala.annotation.tailrec
    def updateDp(currentRowIndex: Int, prevRowArray: Array[Long]): Array[Array[Long]] = {
      if (currentRowIndex > count) {
        dp
      } else {
        // この行で取り扱う品物の重さと価値
        val (w, v) = weightAndValues(currentRowIndex - 1)
        // この行の更新後のArray
        val currentRowArray = (for (i <- 0 to maxWeight) yield {
          // 前の行の数値をそのまま使う場合
          val firstCandidateValue = prevRowArray(i)
          // 価値を加算する場合。Arrayの範囲内に有効な価値数値があるかを確認。もしあれば価値を加算し、なければデフォルトの価値数値を使用。
          val secondCandidateValue = if (i - w >= 0 && prevRowArray(i - w) >= 0) prevRowArray(i - w) + v else -1
          // 価値の値が大きい方を採用
          math.max(firstCandidateValue, secondCandidateValue)
        }).toArray
        // 行を更新
        dp(currentRowIndex) = currentRowArray
        // 次の行の更新へ
        updateDp(currentRowIndex + 1, currentRowArray)
      }
    }

    // 一番最後の行の中の最大値が最大価値
    updateDp(1, dp(0))(count).max
  }


  // 二次元のDP(3):最長共通部分列問題
  def lcs = {
    val scanner = new java.util.Scanner(System.in)
    val firstChars = scanner.next().toVector
    val secondChars = scanner.next().toVector

    // dpを初期化
    val dp = Array.ofDim[Int](firstChars.length + 1, secondChars.length + 1)
    // dpを更新していく。indexが1の行から順に
    for (rowIndex <- 1 to firstChars.length) {
      for (colIndex <- 1 to secondChars.length) {
        // このrowIndexとcolIndexのマスに入れる値を決める
        val value = {
          // 1つ左または1つ上のマスの値の内大きい方
          val tmpMax = math.max(dp(rowIndex)(colIndex - 1), dp(rowIndex - 1)(colIndex))
          // このマスの行と列の文字が一致する場合は、一つ斜め上の値+1とtmpMaxの内大きい方の値。そうでなければtempMax
          // firstChars,secondChars共にindexが0が一文字目なので、文字を参照するindexの場合は行、列のindex-1となる
          if (firstChars(rowIndex - 1) == secondChars(colIndex - 1)) {
            math.max(dp(rowIndex - 1)(colIndex - 1) + 1, tmpMax)
          } else {
            tmpMax
          }
        }
        dp(rowIndex)(colIndex) = value
      }
    }

    dp(firstChars.length)(secondChars.length)
  }


}
