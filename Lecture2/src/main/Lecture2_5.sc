object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x - y - z
  new Rational(10,4)
  y + y
  x < y
  y < x
  y.max(x)
  x.max(y)
  -y
  new Rational(3) + new Rational(-8)
}

class Rational(x:Int, y:Int){
  require(y != 0, "Denominator must be non-zero.")

  def this(x: Int) = this(x, 1)

  private def gcd(a:Int, b:Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational =
    new Rational(-numer, denom)

  def -(that: Rational) =
    this + -that

  def <(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  override def toString = numer + "/" + denom
}