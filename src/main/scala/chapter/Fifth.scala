package chapter

import scala.annotation.tailrec

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


  // 最大公約数
  // ユークリッドの互除法と呼ばれるアルゴリズム。
  // 大きい方の数を小さい方の数で割った余りに変更することを繰り返し、片方の数がゼロになったら終了。もう片方の数が答え。
  def calculateGCD = {
    val scanner = new java.util.Scanner(System.in)
    val Vector(a, b) = Vector.fill(2)(scanner.nextInt())

    def f(a: Int, b: Int): Int = {
      val (bigger, smaller) = if (a >= b) (a, b) else (b, a)
      if (smaller == 0) bigger else {
        val remainder = bigger % smaller
        f(smaller, remainder)
      }
    }

    f(a, b)
  }


  // 余りの計算(1):基本
  // 足し算、引き算、掛け算では、好きなタイミングで余りをとっても答えは変わらないという性質がある。
  // そのため例えば、それぞれの数の余りをとって、その余りの数同士で算術を行う、ということでも余りを求められる。
  def blackboard = {
    val scanner = new java.util.Scanner(System.in)
    val n = scanner.nextInt()
    val inputs = for (_ <- 1 to n) yield {
      val t = scanner.next()
      val a = scanner.nextInt()
      (t, a)
    }

    val results = inputs.scanLeft(0) { case (prev, (t, a)) =>
      val tmp = if (t == "+") prev + a else if (t == "-") prev - a else prev * a
      (if (tmp < 0) tmp + 10000 else tmp) % 10000 // 計算の途中で0を下回った場合は100000を加算した上で除算をする
    }

    results.tail // scanLeftで先頭に追加された0は不要なのでtailを使用
  }


  // 余りの計算(2):累乗
  def power = {
    val scanner = new java.util.Scanner(System.in)
    val Vector(a, b) = Vector.fill(2)(scanner.nextInt())

    // aのb乗をmで割った余りを返す関数
    def powerImpl(a: Long, b: Long, m: Long) = {
      /**
       * @param powerOfTwo 2の乗数
       * @param aToPowerOfTwo aの(2のpowerOfTwo乗)乗
       * @param accumAnswer これまでの計算結果。余り
       * @return
       */
      @scala.annotation.tailrec
      def f(powerOfTwo: Int, aToPowerOfTwo: Long, accumAnswer: Long): Long = {
        if (powerOfTwo >= 30) { // bの最大値は10の9条。そしてこれは2の30乗未満なので。
          accumAnswer
        } else {
          // すなわち1×2のpowerOfTwo乗
          val divisor = 1L << powerOfTwo
          // bがdivisorの倍数であるか。bをビット表現にしたときにpowerOfTwo番目のビットが1かどうかをチェックしている。
          val isMultipleOfDivisor = (b / divisor) % 2 == 1
          // これまでの計算結果を(上記変数がtrueの場合に)更新するためのもの。これまでの計算結果に今回判明した約数を乗じて余りを求めている。
          val nextAnswer = if (isMultipleOfDivisor) (accumAnswer * aToPowerOfTwo) % m else accumAnswer
          // 次に使用される2のpowerOfTwo乗の値。効率的な累乗計算のためのステップである。都度都度1乗から順に始めなくてもいいように。
          // %mとしているのは、aToPowerOfTwoの2乗を計算するときに、計算結果が大きくなりすぎてオーバーフローを引き起こすことを防ぐためです。
          // これにより、aToPowerOfTwo の 2 乗を m で割った余りを効率的に計算することができる。
          //なお、この方法は、(a * b) % m が (a % m * b % m) % m と等しいという、モジュラ演算の性質を利用している。
          val nextAToPowerOfTwo = (aToPowerOfTwo * aToPowerOfTwo) % m
          f(powerOfTwo + 1, nextAToPowerOfTwo, nextAnswer)
        }
      }

      f(0, a, 1)
    }

    powerImpl(a, b, 1000000007)
  }

}
