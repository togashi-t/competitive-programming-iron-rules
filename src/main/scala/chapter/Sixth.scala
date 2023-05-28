package chapter

object Sixth {

  // 偶奇を考える
  def travel = {
    val scanner = new java.util.Scanner(System.in)
    val Vector(n, k) = Vector.fill(2)(scanner.nextInt())

    val shortest = n * 2 - 2
    val res = if (k >= shortest && k % 2 == 0) "Yes" else "No"
    res
  }


  // 足された回数を考える
  // 主客転倒テクニック。視点を変えて楽に数え上げをしよう、ということ。
  // 具体的に言うと「各事象がどれだけ合計に寄与、貢献しているかを考える」ということ。
  def travel2 = {
    val scanner = new java.util.Scanner(System.in)
    val Vector(n, m, b) = Vector.fill(3)(scanner.nextLong())
    val fromStationMinutesVector = Vector.fill(n.toInt)(scanner.nextLong())
    val fromBusStopMinutesVector = Vector.fill(m.toInt)(scanner.nextLong())

    fromStationMinutesVector.sum * m + b * (n * m) + fromBusStopMinutesVector.sum * n
  }


  // 上限値を考える
  def blackCompany1 = {
    val scanner = new java.util.Scanner(System.in)
    val List(d, n) = List.fill(2)(scanner.nextInt())
    val lrhList = (for (_ <- 1 to n) yield List.fill(3)(scanner.nextInt())).toList

    // 各日の上限時間を整理するArray。初期値は24時間としておく。
    // ただし、後段の処理時に何日目の数値とindexが一致していた方がわかりやすいので、先頭に0の要素を入れている。
    val dayMaxHourArray = Array.tabulate(d + 1)(i => if (i == 0) 0 else 24)

    lrhList.foreach { case (l :: r :: h :: Nil) =>
      for (x <- l to r) {
        val minHour = math.min(dayMaxHourArray(x), h)
        dayMaxHourArray(x) = minHour
      }
    }

    dayMaxHourArray.sum
  }


  // 一手先を考える
  // 区間スケジューリング問題
  def intervalScheduling = {
    val scanner = new java.util.Scanner(System.in)
    val n = scanner.nextInt()
    val lrList = (for (_ <- 1 to n) yield List.fill(2)(scanner.nextInt())).toList

    // リストの要素を終了時間が早い順に並べる。後の処理で要素へのアクセス効率が良くなるよう、Arrayに変換しておく。
    val sortedLrArray = lrList.sortBy { case _ :: r :: Nil => r }.toArray
    // 回数を算出
    val (_, count) = (1 to n).foldLeft((0, 0)) { case ((currentTime, tmpCount), i) =>
      val l :: r :: Nil = sortedLrArray(i - 1)
      // 現在時刻が上映開始時刻以前の場合は、次処理時の現在時刻と見た回数を更新
      if (currentTime <= l) (r, tmpCount + 1) else (currentTime, tmpCount)
    }
    count
  }


  // 個数を考える
  def triangle = {
    val scanner = new java.util.Scanner(System.in)
    val n = scanner.nextInt()
    val numbers = List.fill(n)(scanner.nextInt())

    // リストの要素の数値毎の数をまとめておく
    val numberCountMap = numbers.groupBy(identity).view.mapValues(_.size).toMap
    // あり得る数値毎に組み合わせの数を算出して足し合わせていく
    (1 to 100).foldLeft(0L) { case (acc, length) =>
      // 指定の数値がいくつ存在するのか
      val targetNumberCount = numberCountMap.getOrElse(length, 0)
      if (targetNumberCount < 3) acc else acc + getCombinationCount(targetNumberCount, 3)
    }
  }

  // n個の中からr個とる組み合わせが何通りあるかを返す
  private def getCombinationCount(n: Long, r: Long): Long = {
    if (r == 0 || r == n) 1
    else if (r > n / 2) getCombinationCount(n, n - r) // 計算量を減らすため
    else (n * getCombinationCount(n - 1, r - 1)) / r // 大きな値同士の計算(=オーバーフロー発生)を回避するためこうしている
  }

}
