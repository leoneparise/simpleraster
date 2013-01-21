package climadata.raster

import scala.language.implicitConversions

trait ResultType[@specialized A, @specialized B, @specialized R] {
  def convertA(r:Raster[A])(implicit b:RasterBuilder[A, R]):Raster[R]
  def convertB(r:Raster[B])(implicit b:RasterBuilder[B, R]):Raster[R]
}

trait ResultTypes {
  // Byte Convertions
  implicit object ByteByte extends ResultType[Byte, Byte, Byte] {
    def convertA(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Byte]):Raster[Byte] = r
    def convertB(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Byte]):Raster[Byte] = r
  }

  implicit object ByteShort extends ResultType[Byte, Short, Short] {
    def convertA(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Short]):Raster[Short] = r.map(_.toShort)
    def convertB(r:Raster[Short])(implicit b:RasterBuilder[Short, Short]):Raster[Short] = r
  }

  implicit object ByteInt extends ResultType[Byte, Int, Int] {
    def convertA(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Int]):Raster[Int] = r.map(_.toInt)
    def convertB(r:Raster[Int])(implicit b:RasterBuilder[Int, Int]):Raster[Int] = r
  }

  implicit object ByteFloat extends ResultType[Byte, Float, Float] {
    def convertA(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Float]):Raster[Float] = r.map(_.toFloat)
    def convertB(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
  }

  implicit object ByteDouble extends ResultType[Byte, Double, Double] {
    def convertA(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Double]):Raster[Double] = r.map(_.toDouble)
    def convertB(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
  }

  // Short Convertions
  implicit object ShortByte extends ResultType[Short, Byte, Short] {
    def convertA(r:Raster[Short])(implicit b:RasterBuilder[Short, Short]):Raster[Short] = r
    def convertB(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Short]):Raster[Short] = r.map(_.toShort)
  }

  implicit object ShortShort extends ResultType[Short, Short, Short] {
    def convertA(r:Raster[Short])(implicit b:RasterBuilder[Short, Short]):Raster[Short] = r
    def convertB(r:Raster[Short])(implicit b:RasterBuilder[Short, Short]):Raster[Short] = r
  }

  implicit object ShortInt extends ResultType[Short, Int, Int] {
    def convertA(r:Raster[Short])(implicit b:RasterBuilder[Short, Int]):Raster[Int] = r.map(_.toInt)
    def convertB(r:Raster[Int])(implicit b:RasterBuilder[Int, Int]):Raster[Int] = r
  }

  implicit object ShortFloat extends ResultType[Short, Float, Float] {
    def convertA(r:Raster[Short])(implicit b:RasterBuilder[Short, Float]):Raster[Float] = r.map(_.toFloat)
    def convertB(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
  }

  implicit object ShortDouble extends ResultType[Short, Double, Double] {
    def convertA(r:Raster[Short])(implicit b:RasterBuilder[Short, Double]):Raster[Double] = r.map(_.toDouble)
    def convertB(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
  }

  // Int Convertions
  implicit object IntByte extends ResultType[Int, Byte, Int] {
    def convertA(r:Raster[Int])(implicit b:RasterBuilder[Int, Int]):Raster[Int] = r
    def convertB(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Int]):Raster[Int] = r.map(_.toInt)
  }

  implicit object IntShort extends ResultType[Int, Short, Int] {
    def convertA(r:Raster[Int])(implicit b:RasterBuilder[Int, Int]):Raster[Int] = r
    def convertB(r:Raster[Short])(implicit b:RasterBuilder[Short, Int]):Raster[Int] = r.map(_.toInt)
  }

  implicit object IntInt extends ResultType[Int, Int, Int]{
    def convertA(r:Raster[Int])(implicit b:RasterBuilder[Int, Int]):Raster[Int] = r
    def convertB(r:Raster[Int])(implicit b:RasterBuilder[Int, Int]):Raster[Int] = r
  }

  implicit object IntFloat extends ResultType[Int, Float, Float]{
    def convertA(r:Raster[Int])(implicit b:RasterBuilder[Int, Float]):Raster[Float] = r.map(_.toFloat)
    def convertB(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
  }

  implicit object IntDouble extends ResultType[Int, Double, Double]{
    def convertA(r:Raster[Int])(implicit b:RasterBuilder[Int, Double]):Raster[Double] = r.map(_.toDouble)
    def convertB(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
  }

  // Float Convertions
  implicit object FloatByte extends ResultType[Float, Byte, Float] {
    def convertA(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
    def convertB(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Float]):Raster[Float] = r.map(_.toFloat)
  }

  implicit object FloatShort extends ResultType[Float, Short, Float] {
    def convertA(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
    def convertB(r:Raster[Short])(implicit b:RasterBuilder[Short, Float]):Raster[Float] = r.map(_.toFloat)
  }

  implicit object FloatInt extends ResultType[Float, Int, Float] {
    def convertA(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
    def convertB(r:Raster[Int])(implicit b:RasterBuilder[Int, Float]):Raster[Float] = r.map(_.toFloat)
  }

  implicit object FloatFloat extends ResultType[Float, Float, Float] {
    def convertA(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
    def convertB(r:Raster[Float])(implicit b:RasterBuilder[Float, Float]):Raster[Float] = r
  }

  implicit object FloatDouble extends ResultType[Float, Double, Double] {
    def convertA(r:Raster[Float])(implicit b:RasterBuilder[Float, Double]):Raster[Double] = r.map(_.toDouble)
    def convertB(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
  }

  // Double Convertions
  implicit object DoubleByte   extends ResultType[Double, Byte, Double] {
    def convertA(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
    def convertB(r:Raster[Byte])(implicit b:RasterBuilder[Byte, Double]):Raster[Double] = r.map(_.toDouble)
  }

  implicit object DoubleShort  extends ResultType[Double, Short, Double] {
    def convertA(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
    def convertB(r:Raster[Short])(implicit b:RasterBuilder[Short, Double]):Raster[Double] = r.map(_.toDouble)
  }

  implicit object DoubleInt    extends ResultType[Double, Int, Double] {
    def convertA(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
    def convertB(r:Raster[Int])(implicit b:RasterBuilder[Int, Double]):Raster[Double] = r.map(_.toDouble)
  }

  implicit object DoubleFloat  extends ResultType[Double, Float, Double] {
    def convertA(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
    def convertB(r:Raster[Float])(implicit b:RasterBuilder[Float, Double]):Raster[Double] = r.map(_.toDouble)
  }

  implicit object DoubleDouble extends ResultType[Double, Double, Double] {
    def convertA(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
    def convertB(r:Raster[Double])(implicit b:RasterBuilder[Double, Double]):Raster[Double] = r
  }
}
