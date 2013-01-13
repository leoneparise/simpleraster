package climadata.raster

import scala.reflect.Manifest

object ConstantRaster {
  def apply[T:Manifest](const:T, cols:Int, rows:Int):Raster[T] = {
    const match {
      case c:Byte => ByteConstantRaster(c, cols, rows).asInstanceOf[Raster[T]]
      case c:Short => ShortConstantRaster(c, cols, rows).asInstanceOf[Raster[T]]
      case c:Int => IntConstantRaster(c, cols, rows).asInstanceOf[Raster[T]]
      case c:Float => FloatConstantRaster(c, cols, rows).asInstanceOf[Raster[T]]
      case c:Double => DoubleConstantRaster(c, cols, rows).asInstanceOf[Raster[T]]
    }
  }
}

abstract class ConstantRaster[T](val c:T, val cols:Int, val rows:Int) extends Raster[T] {
  override def size = cols * rows

  override def apply(col:Int, row:Int) =
    if (row < 0 || row >= rows || col < 0 || col >= cols || row * cols + col >= size)
      throw new ArrayIndexOutOfBoundsException("Array index out of range: (%d, %d)".format(row, col))
    else c
}

import RasterUtils._

private case class ByteConstantRaster(_c:Byte, _cols:Int, _rows:Int) extends ConstantRaster[Byte](_c, _cols, _rows) {
  private[raster] def data = Array(_c)

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
    new ShortConstantRaster(if(isNodata(c)) shortNodata else c.toShort, cols, rows)

  def toInt: Raster[Int] =
    new IntConstantRaster(if(isNodata(c)) intNodata else c.toInt, cols, rows)

  def toFloat: Raster[Float] =
    new FloatConstantRaster(if(isNodata(c)) floatNodata else c.toFloat, cols, rows)

  def toDouble: Raster[Double] =
    new DoubleConstantRaster(if(isNodata(c)) doubleNodata else c.toDouble, cols, rows)

  def +(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a + b).toByte)

  def -(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a - b).toByte)

  def /(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a / b).toByte)

  def *(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a * b).toByte)

  def %(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a % b).toByte)

  def |(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a | b).toByte)

  def &(rhs:Raster[Byte]) = combine(rhs)((a,b) => (a & b).toByte)
}

private case class ShortConstantRaster(_c:Short, _cols:Int, _rows:Int) extends ConstantRaster[Short](_c, _cols, _rows) {
  private[raster] def data = Array(_c)

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
    new ByteConstantRaster(if(isNodata(c)) byteNodata else c.toByte, cols, rows)

  def toShort: Raster[Short] = this

  def toInt: Raster[Int] =
    new IntConstantRaster(if(isNodata(c)) intNodata else c.toInt, cols, rows)

  def toFloat: Raster[Float] =
    new FloatConstantRaster(if(isNodata(c)) floatNodata else c.toFloat, cols, rows)

  def toDouble: Raster[Double] =
    new DoubleConstantRaster(if(isNodata(c)) doubleNodata else c.toDouble, cols, rows)

  def +(rhs:Raster[Short]) = combine(rhs)((a,b) => (a + b).toShort)

  def -(rhs:Raster[Short]) = combine(rhs)((a,b) => (a - b).toShort)

  def /(rhs:Raster[Short]) = combine(rhs)((a,b) => (a / b).toShort)

  def *(rhs:Raster[Short]) = combine(rhs)((a,b) => (a * b).toShort)

  def %(rhs:Raster[Short]) = combine(rhs)((a,b) => (a % b).toShort)

  def |(rhs:Raster[Short]) = combine(rhs)((a,b) => (a | b).toShort)

  def &(rhs:Raster[Short]) = combine(rhs)((a,b) => (a & b).toShort)
}

private case class IntConstantRaster(_c:Int, _cols:Int, _rows:Int) extends ConstantRaster[Int](_c, _cols, _rows) {
  private[raster] def data = Array(_c)

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
    new ByteConstantRaster(if(isNodata(c)) byteNodata else c.toByte, cols, rows)

  def toShort: Raster[Short] =
    new ShortConstantRaster(if(isNodata(c)) shortNodata else c.toShort, cols, rows)

  def toInt: Raster[Int] = this

  def toFloat: Raster[Float] =
    new FloatConstantRaster(if(isNodata(c)) floatNodata else c.toFloat, cols, rows)

  def toDouble: Raster[Double] =
    new DoubleConstantRaster(if(isNodata(c)) doubleNodata else c.toDouble, cols, rows)

  def +(rhs:Raster[Int]) = combine(rhs)(_ + _)

  def -(rhs:Raster[Int]) = combine(rhs)(_ - _)

  def /(rhs:Raster[Int]) = combine(rhs)(_ / _)

  def *(rhs:Raster[Int]) = combine(rhs)(_ * _)

  def %(rhs:Raster[Int]) = combine(rhs)(_ % _)

  def |(rhs:Raster[Int]) = combine(rhs)(_ | _)

  def &(rhs:Raster[Int]) = combine(rhs)(_ & _)
}

private case class FloatConstantRaster(_c:Float, _cols:Int, _rows:Int) extends ConstantRaster[Float](_c, _cols, _rows) {
  private[raster] def data = Array(_c)

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
    new ByteConstantRaster(if(isNodata(c)) byteNodata else c.toByte, cols, rows)

  def toShort: Raster[Short] =
    new ShortConstantRaster(if(isNodata(c)) shortNodata else c.toShort, cols, rows)

  def toInt: Raster[Int] =
    new IntConstantRaster(if(isNodata(c)) intNodata else c.toInt, cols, rows)

  def toFloat: Raster[Float] = this

  def toDouble: Raster[Double] =
    new DoubleConstantRaster(if(isNodata(c)) doubleNodata else c.toDouble, cols, rows)

  def +(rhs:Raster[Float]) = combine(rhs)((a,b) => (a + b).toFloat)

  def -(rhs:Raster[Float]) = combine(rhs)((a,b) => (a - b).toFloat)

  def /(rhs:Raster[Float]) = combine(rhs)((a,b) => (a / b).toFloat)

  def *(rhs:Raster[Float]) = combine(rhs)((a,b) => (a * b).toFloat)

  def %(rhs:Raster[Float]) = throw new UnsupportedOperationException

  def |(rhs:Raster[Float]) = throw new UnsupportedOperationException

  def &(rhs:Raster[Float]) = throw new UnsupportedOperationException
}

private case class DoubleConstantRaster(_c:Double, _cols:Int, _rows:Int) extends ConstantRaster[Double](_c, _cols, _rows) {
  private[raster] def data = Array(_c)

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
    new ByteConstantRaster(if(isNodata(c)) byteNodata else c.toByte, cols, rows)

  def toShort: Raster[Short] =
    new ShortConstantRaster(if(isNodata(c)) shortNodata else c.toShort, cols, rows)

  def toInt: Raster[Int] =
    new IntConstantRaster(if(isNodata(c)) intNodata else c.toInt, cols, rows)

  def toFloat: Raster[Float] =
    new FloatConstantRaster(if(isNodata(c)) floatNodata else c.toFloat, cols, rows)

  def toDouble: Raster[Double] = this

  def +(rhs:Raster[Double]) = combine(rhs)(_ + _)

  def -(rhs:Raster[Double]) = combine(rhs)(_ - _)

  def /(rhs:Raster[Double]) = combine(rhs)(_ / _)

  def *(rhs:Raster[Double]) = combine(rhs)(_ * _)

  def %(rhs:Raster[Double]) = throw new UnsupportedOperationException

  def |(rhs:Raster[Double]) = throw new UnsupportedOperationException

  def &(rhs:Raster[Double]) = throw new UnsupportedOperationException
}