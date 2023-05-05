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




}
