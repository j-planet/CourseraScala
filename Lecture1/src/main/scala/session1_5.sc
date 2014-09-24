object session1_5 {
  1+3

  def abs(x: Double) = if (x<0) -x else x

  def sqrt(x: Double) =
  {
    def sqrtIter(guess: Double): Double =
      if (x < 0) throw new Exception("The square root of negative numbers cannot be evaluated.")
      else if (x == 0) 0
      else if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess*guess - x) < 0.001*x

    def improve(guess: Double) = (guess + x/guess) / 2

    sqrtIter(1.0)
  }


  sqrt(4)
  sqrt(3)
}