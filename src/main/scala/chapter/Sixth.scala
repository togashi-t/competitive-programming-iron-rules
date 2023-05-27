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

}
