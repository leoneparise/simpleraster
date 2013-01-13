package climadata.raster

import scala.reflect.Manifest
import RasterUtils._

object Raster {
  def apply[T:Manifest](array:Array[T], cols:Int, rows:Int):Raster[T] = {
    array match {
      case raster:Array[Byte] => ByteRaster(raster, cols, rows).asInstanceOf[Raster[T]]
      case raster:Array[Short] => ShortRaster(raster, cols, rows).asInstanceOf[Raster[T]]
      case raster:Array[Int] => IntRaster(raster, cols, rows).asInstanceOf[Raster[T]]
      case raster:Array[Float] => FloatRaster(raster, cols, rows).asInstanceOf[Raster[T]]
      case raster:Array[Double] => DoubleRaster(raster, cols, rows).asInstanceOf[Raster[T]]
      case _ => throw new IllegalArgumentException("Type not supported")
    }
  }
}

trait Raster[T] {
  private[raster] def data:Array[T]

  def cols:Int

  def rows:Int

  def size = data.size

  def apply(row:Int, col:Int) =
    if (row < 0 || row >= rows || col < 0 || col >= cols || row * cols + col >= size)
      throw new ArrayIndexOutOfBoundsException("Array index out of range: (%d, %d)".format(row, col))
    else
      data(row * cols + col)

  def combine(raster:Raster[T])(f: (T, T) => T):Raster[T]

  def toByte:Raster[Byte]
  def toShort:Raster[Short]
  def toInt:Raster[Int]
  def toFloat:Raster[Float]
  def toDouble:Raster[Double]

  def +(rhs:Raster[T]):Raster[T]
  def -(rhs:Raster[T]):Raster[T]
  def *(rhs:Raster[T]):Raster[T]
  def /(rhs:Raster[T]):Raster[T]
  def %(rhs:Raster[T]):Raster[T]
  def |(rhs:Raster[T]):Raster[T]
  def &(rhs:Raster[T]):Raster[T]
}

private case class ByteRaster(
  raster:Array[Byte],
  val cols:Int,
  val rows:Int
) extends Raster[Byte] {

  if (cols * rows > size)
    throw new IllegalArgumentException("Rows times cols must be equals raster size")

  private[raster] def data = raster

  def combine(raster:Raster[Byte])(f: (Byte, Byte) => Byte):Raster[Byte] = {
    val array =
      (for{row <- 0 until rows
           col <- 0 until cols
           r1 = this(row, col)
           r2 = raster(row, col)
      } yield {
        if(isNodata(r1) || isNodata(r2)) byteNodata else f(r1, r2)
      }).toArray

    new ByteRaster(array, rows ,cols)
  }

  def toByte: Raster[Byte] = this

  def toShort: Raster[Short] =
    new ShortRaster(data.map(v => if(isNodata(v)) shortNodata else v.toShort), cols, rows)

  def toInt:Raster[Int] =
    new IntRaster(data.map(v => if(isNodata(v)) intNodata else v.toInt), cols, rows)

  def toFloat:Raster[Float] =
    new FloatRaster(data.map(v => if(isNodata(v)) floatNodata else v.toFloat), cols, rows)

  def toDouble:Raster[Double] =
    new DoubleRaster(data.map(v => if(isNodata(v)) doubleNodata else v.toDouble), cols, rows)

  def +(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a + b).toByte)

  def -(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a - b).toByte)

  def /(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a / b).toByte)

  def *(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a * b).toByte)

  def %(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a % b).toByte)

  def |(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a | b).toByte)

  def &(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a & b).toByte)
}

private case class ShortRaster(
  raster:Array[Short],
  val cols:Int,
  val rows:Int
) extends Raster[Short] {

  if (cols * rows > size)
    throw new IllegalArgumentException("Rows times cols must be equals raster size")

  private[raster] def data = raster

  def combine(raster:Raster[Short])(f: (Short, Short) => Short):Raster[Short] = {
    val array =
      (for{row <- 0 until rows
           col <- 0 until cols
           r1 = this(row, col)
           r2 = raster(row, col)
      } yield {
        if(isNodata(r1) || isNodata(r2)) shortNodata else f(r1, r2)
      }).toArray

    new ShortRaster(array, rows ,cols)
  }

  def toByte: Raster[Byte] =
    new ByteRaster(data.map(v => if(isNodata(v)) byteNodata else v.toByte), cols, rows)

  def toShort: Raster[Short] = this

  def toInt:Raster[Int] =
    new IntRaster(data.map(v => if(isNodata(v)) intNodata else v.toInt), cols, rows)

  def toFloat:Raster[Float] =
    new FloatRaster(data.map(v => if(isNodata(v)) floatNodata else v.toFloat), cols, rows)

  def toDouble:Raster[Double] =
    new DoubleRaster(data.map(v => if(isNodata(v)) doubleNodata else v.toDouble), cols, rows)

  def +(rhs:Raster[Short]) = combine(rhs)((a,b) => (a + b).toShort)

  def -(rhs:Raster[Short]) = combine(rhs)((a,b) => (a - b).toShort)

  def /(rhs:Raster[Short]) = combine(rhs)((a,b) => (a / b).toShort)

  def *(rhs:Raster[Short]) = combine(rhs)((a,b) => (a * b).toShort)

  def %(rhs:Raster[Short]) = combine(rhs)((a,b) => (a % b).toShort)

  def |(rhs:Raster[Short]) = combine(rhs)((a,b) => (a | b).toShort)

  def &(rhs:Raster[Short]) = combine(rhs)((a,b) => (a & b).toShort)
}

private case class IntRaster(
  raster:Array[Int],
  val cols:Int,
  val rows:Int
) extends Raster[Int] {

  if (cols * rows > size)
    throw new IllegalArgumentException("Rows times cols must be equals raster size")

  private[raster] def data = raster

  def combine(raster:Raster[Int])(f: (Int, Int) => Int):Raster[Int] = {
    val array =
      (for{row <- 0 until rows
           col <- 0 until cols
           r1 = this(row, col)
           r2 = raster(row, col)
      } yield {
        if(isNodata(r1) || isNodata(r2)) intNodata else f(r1, r2)
      }).toArray

    new IntRaster(array, rows ,cols)
  }

  def toByte: Raster[Byte] =
    new ByteRaster(data.map(v => if(isNodata(v)) byteNodata else v.toByte), cols, rows)

  def toShort: Raster[Short] =
    new ShortRaster(data.map(v => if(isNodata(v)) shortNodata else v.toShort), cols, rows)

  def toInt:Raster[Int] = this

  def toFloat:Raster[Float] =
    new FloatRaster(data.map(v => if(isNodata(v)) floatNodata else v.toFloat), cols, rows)

  def toDouble:Raster[Double] =
    new DoubleRaster(data.map(v => if(isNodata(v)) doubleNodata else v.toDouble), cols, rows)

  def +(rhs:Raster[Int]) = combine(rhs)(_ + _)

  def -(rhs:Raster[Int]) = combine(rhs)(_ - _)

  def /(rhs:Raster[Int]) = combine(rhs)(_ / _)

  def *(rhs:Raster[Int]) = combine(rhs)(_ * _)

  def %(rhs:Raster[Int]) = combine(rhs)(_ % _)

  def |(rhs:Raster[Int]) = combine(rhs)(_ | _)

  def &(rhs:Raster[Int]) = combine(rhs)(_ & _)
}


private case class FloatRaster(
  raster:Array[Float],
  val cols:Int,
  val rows:Int
) extends Raster[Float] {

  if (cols * rows > size)
    throw new IllegalArgumentException("Rows times cols must be equals raster size")

  private[raster] def data = raster

  def combine(raster:Raster[Float])(f: (Float, Float) => Float):Raster[Float] = {
    val array =
      (for{row <- 0 until rows
           col <- 0 until cols
           r1 = this(row, col)
           r2 = raster(row, col)
      } yield {
        if(isNodata(r1) || isNodata(r2)) floatNodata else f(r1, r2)
      }).toArray

    new FloatRaster(array, rows ,cols)
  }

  def toByte: Raster[Byte] =
    new ByteRaster(data.map(v => if(isNodata(v)) byteNodata else v.toByte), cols, rows)

  def toShort: Raster[Short] =
    new ShortRaster(data.map(v => if(isNodata(v)) shortNodata else v.toShort), cols, rows)

  def toInt:Raster[Int] =
    new IntRaster(data.map(v => if(isNodata(v)) intNodata else v.toInt), cols, rows)

  def toFloat:Raster[Float] = this

  def toDouble:Raster[Double] =
    new DoubleRaster(data.map(v => if(isNodata(v)) doubleNodata else v.toDouble), cols, rows)

  def +(rhs:Raster[Float]) = combine(rhs)((a,b) => (a + b).toFloat)

  def -(rhs:Raster[Float]) = combine(rhs)((a,b) => (a - b).toFloat)

  def /(rhs:Raster[Float]) = combine(rhs)((a,b) => (a / b).toFloat)

  def *(rhs:Raster[Float]) = combine(rhs)((a,b) => (a * b).toFloat)

  def %(rhs:Raster[Float]) = throw new UnsupportedOperationException

  def |(rhs:Raster[Float]) = throw new UnsupportedOperationException

  def &(rhs:Raster[Float]) = throw new UnsupportedOperationException
}

private case class DoubleRaster(
  raster:Array[Double],
  val cols:Int,
  val rows:Int
) extends Raster[Double] {

  if (cols * rows > size)
    throw new IllegalArgumentException("Rows times cols must be equals raster size")

  private[raster] def data = raster

  def combine(raster:Raster[Double])(f: (Double, Double) => Double):Raster[Double] = {
    val array =
    (for{row <- 0 until rows
         col <- 0 until cols
         r1 = this(row, col)
         r2 = raster(row, col)
    } yield {
      if(isNodata(r1) || isNodata(r2)) doubleNodata else f(r1, r2)
    }).toArray

    new DoubleRaster(array, rows ,cols)
  }

  def toByte: Raster[Byte] =
    new ByteRaster(data.map(v => if(isNodata(v)) byteNodata else v.toByte), cols, rows)

  def toShort: Raster[Short] =
    new ShortRaster(data.map(v => if(isNodata(v)) shortNodata else v.toShort), cols, rows)

  def toInt:Raster[Int] =
    new IntRaster(data.map(v => if(isNodata(v)) intNodata else v.toInt), cols, rows)

  def toFloat:Raster[Float] =
    new FloatRaster(data.map(v => if(isNodata(v)) floatNodata else v.toFloat), cols, rows)

  def toDouble:Raster[Double] = this

  def +(rhs:Raster[Double]) = combine(rhs)(_ + _)

  def -(rhs:Raster[Double]) = combine(rhs)(_ - _)

  def /(rhs:Raster[Double]) = combine(rhs)(_ / _)

  def *(rhs:Raster[Double]) = combine(rhs)(_ * _)

  def %(rhs:Raster[Double]) = throw new UnsupportedOperationException

  def |(rhs:Raster[Double]) = throw new UnsupportedOperationException

  def &(rhs:Raster[Double]) = throw new UnsupportedOperationException
}