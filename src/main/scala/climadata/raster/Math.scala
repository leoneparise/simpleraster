package climadata.raster

object Math {
  def log(r:Raster[Double]):Raster[Double] = r.map(scala.math.log(_))

  def log10(r:Raster[Double]):Raster[Double] = r.map(scala.math.log10(_))

  def sin(r:Raster[Double]):Raster[Double] = r.map(scala.math.sin(_))

  def cos(r:Raster[Double]):Raster[Double] = r.map(scala.math.cos(_))

  def tan(r:Raster[Double]):Raster[Double] = r.map(scala.math.tan(_))

  def exp(r:Raster[Double]):Raster[Double] = r.map(scala.math.exp(_))

  def pow(r:Raster[Double], y:Double):Raster[Double] = r.map(scala.math.pow(_, y))

  def sqrt(r:Raster[Double]):Raster[Double] = r.map(scala.math.sqrt(_))

  def cbrt(r:Raster[Double]):Raster[Double] = r.map(scala.math.cbrt(_))

  def max(r1:Raster[Int], r2:Raster[Int]):Raster[Int] =
    r1.combine(r2)((r1, r2) => scala.math.max(r1, r2))

  def max(r1:Raster[Float], r2:Raster[Float])(implicit i1:DI):Raster[Float] =
    r1.combine(r2)((r1, r2) => scala.math.max(r1, r2))

  def max(r1:Raster[Double], r2:Raster[Double])(implicit i1:DI, i2:DI):Raster[Double] =
    r1.combine(r2)((r1, r2) => scala.math.max(r1, r2))

  def min(r1:Raster[Int], r2:Raster[Int]):Raster[Int] =
    r1.combine(r2)((r1, r2) => scala.math.min(r1, r2))

  def min(r1:Raster[Float], r2:Raster[Float])(implicit i1:DI):Raster[Float] =
    r1.combine(r2)((r1, r2) => scala.math.min(r1, r2))

  def min(r1:Raster[Double], r2:Raster[Double])(implicit i1:DI, i2:DI):Raster[Double] =
    r1.combine(r2)((r1, r2) => scala.math.min(r1, r2))

  def abs(r1:Raster[Int]):Raster[Int] =
    r1.map(scala.math.abs(_))

  def abs(r1:Raster[Float])(implicit i1:DI):Raster[Float] =
    r1.map(scala.math.abs(_))

  def abs(r1:Raster[Double])(implicit i1:DI, i2:DI):Raster[Double] =
    r1.map(scala.math.abs(_))

  def atan(r1:Raster[Double]):Raster[Double] =
    r1.map(scala.math.atan(_))
}
