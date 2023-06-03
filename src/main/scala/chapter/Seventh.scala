package chapter

object Seventh {

  def heuristic1 = {
    val scanner = new java.util.Scanner(System.in)
    val n = scanner.nextInt()
    // 座標とそれが何番目なのかの情報を持つ
    val pointWithNumbers = (for (i <- 1 to n) yield {
      val Array(x, y) = Array.fill(2)(scanner.nextInt())
      ((x, y), i)
    }).toList

    /**
     * 何番目の座標に移動という情報を蓄積していくための関数
     * @param currentPoint 現在の座標
     * @param acc 目的の情報
     * @param remaining 移動対象候補の座標とそれが何番目かの情報のリスト
     * @return
     */
    @scala.annotation.tailrec
    def f(currentPoint: (Int, Int), acc: List[Int], remaining: List[((Int, Int), Int)]): List[Int] = {
      remaining match {
        case Nil =>
          throw new Exception("unexpected")
        case _ @ x :: xs =>
          if (xs.nonEmpty) {
            // 現在の座標
            val (currentX, currentY) = currentPoint
            // 移動候補の座標を近い順に並べ替え
            val sortedRemaining = remaining.sortBy { case ((x, y), _) => math.pow(x - currentX, 2) + math.pow(y - currentY, 2)}
            // 最も近い座標を採用
            val (newCurrentPoint, n) = sortedRemaining.head
            f(newCurrentPoint, n :: acc, sortedRemaining.tail)
          } else {
            val (_, n) = x
            n :: acc
          }
      }
    }

    val (firstPoint, firstN) = pointWithNumbers.head
    f(firstPoint, List(firstN), pointWithNumbers.tail).reverse // 上記処理で逆順になっているのでここで順序を直す
  }

}
