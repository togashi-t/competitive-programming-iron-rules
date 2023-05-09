package chapter

object Fifth {

  // 素数判定
  def primeCheck = {
    val scanner = new java.util.Scanner(System.in)
    val count = scanner.nextInt()
    val numbers = for (i <- 1 to count) yield {
      scanner.nextInt()
    }

    def isPrime(n: Int): Boolean = {
      if (n < 2) { // 2未満の場合は素数でない
        false
      } else if (n == 2) { // 2の場合は素数
        true
      } else if (n % 2 == 0) { // 2以外の偶数の場合は素数ではない
        false
      } else { // それ以外の場合は、自ら以外の奇数で割り切れなければ素数。
        // 割り切れるということは2つの数の乗算の解ということであり、この2つの数の内の一方はnの平方根の値までで網羅できる -> 全ての合成数は2以上√n以下の約数を持つ
        // したがって、平方根の値を上限としていている
        !(for (i <- 3 to math.sqrt(n).toInt by 2) yield n % i).contains(0)
      }
    }

    numbers.map { n =>
      if (isPrime(n)) "Yes" else "No"
    }
  }

}