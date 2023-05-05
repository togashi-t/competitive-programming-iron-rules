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




}
