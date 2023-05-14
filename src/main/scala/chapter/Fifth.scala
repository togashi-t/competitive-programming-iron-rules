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


  // 余りの計算(3):割り算
  def combination = {
    val scanner = new java.util.Scanner(System.in)
    val Vector(n, r) = Vector.fill(2)(scanner.nextInt())
    // 割り算に使用する数
    val m = 1000000007

    // 繰り返し二乗法(べき乗の計算を高速に行うためのアルゴリズム)を実行する関数。
    // 各ステップで余りを計算することで、大きな数の積や冪を計算してもオーバーフローを防止できる。
    def power(x: Long, y: Int, m: Int): Long = {
      if (y == 0) 1
      // yが偶数の場合。計算を(x^2)^(y/2)として行う。 これは、x^y = (x^2)^(y/2)であることを利用している。
      // ここで、x^2 mod mを再帰的に計算し、結果をy/2の指数として計算する。
      else if (y % 2 == 0) power((x * x) % m, y / 2, m)
      // yが奇数の場合。計算をx * (x^2)^((y-1)/2)として行う。これは、x^y = x * x^(y-1)であることを利用している。
      // ここで、x^2 mod mを再帰的に計算し、結果を(y-1)/2の指数として計算する。その後、xを掛けている。
      else (x * power((x * x) % m, y / 2, m)) % m
    }

    // 分子を求める。余りを
    val a = (1 to n).foldLeft(1L) { case (accRemainder, x) => (accRemainder * x) % m }
    // 分母を求める。余りを
    val b = {
      val left = (1 to r).foldLeft(1L) { case (accRemainder, x) => (accRemainder * x) % m }
      val right = (1 to n - r).foldLeft(1L) { case (accRemainder, x) => (accRemainder * x) % m }
      (left * right) % m
    }

    (a * power(b, m - 2, m)) % m
  }


  // 包除原理
  // 例題。3または5で割り切れる数はいくつあるか
  def divisors = {
    val scanner = new java.util.Scanner(System.in)
    val n = scanner.nextLong()

    n / 3 + n / 5  - n / 15
  }


  // ゲーム(1):必勝法
  def game1 = {
    val scanner = new java.util.Scanner(System.in)
    val Vector(n, a, b) = Vector.fill(3)(scanner.nextInt()) // aはbよりも小さい

    // 残りの石の数がいくつ(index番号と一致)の時点で自分の番になったときに勝ち(true)と負け(false)のいずれであるのかを記録する
    val dp = Array.fill(n + 1)(false)
    for (i <- 0 to n) {
      // 自分が石を取る余地があって、取った結果相手の状態が負けに遷移する場合は勝ち。そうでない場合は負け。
      if (i >= a && !dp(i - a)) dp(i) = true
      else if (i >= b && !dp(i - b)) dp(i) = true
      else dp(i) = false
    }

    dp(n)
  }


}
