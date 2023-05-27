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

}
