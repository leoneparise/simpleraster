package climadata.raster

import scala.reflect._
import scala.language.implicitConversions

object Raster {
  def apply[T: ClassTag](data:Array[T], cols:Int, rows:Int):Raster[T] =
    new Raster(data, cols, rows)

  def apply[T: ClassTag](const:T):Raster[T] =
    new ConstantRaster(const)
}

class Raster[T: ClassTag](val data:Array[T], val cols:Int, val rows:Int) {
  if (data.length != size)
    throw new IllegalArgumentException("Data size and raster size must be equals")

  def size = cols * rows

  def apply(row:Int, col:Int) =
    if (row < 0 || row >= rows || col < 0 || col >= cols || row * cols + col >= size)
      throw new ArrayIndexOutOfBoundsException("Array index out of range: (%d, %d)".format(row, col))
    else
      data(row * cols + col)

  def map[B](f:T => B)(implicit b:RasterBuilder[T, B]):Raster[B] = b.map(this, f)

  def combine(raster:Raster[T])(f: (T, T) => T)(implicit b:RasterBuilder[T, T]):Raster[T] = {
    val array =
      (for(i <- 0 until cols; j <- 0 until rows; r1 = this(i, j); r2 = raster(i, j))
       yield if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)).toArray

    Raster(array, cols, rows)
  }

  def +[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:Sum[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)

  def -[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:Sub[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)

  def /[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:Divide[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)

  def *[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:Multiply[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)

  def %[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:Modulus[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)

  def &[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:And[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)

  def |[B, R](raster:Raster[B])(implicit r:ResultType[T, B, R], b1:RasterBuilder[B, R], b2:RasterBuilder[T, R], b3:RasterBuilder[R, R], op:Or[R]):Raster[R] =
    r.convertA(this).combine(r.convertB(raster))(op.handle)
}

class ConstantRaster[T: ClassTag](val const:T) extends Raster[T](Array(const), 1, 1) {
  override def apply(col:Int, row:Int) = const

  override def combine(raster:Raster[T])(f: (T, T) => T)(implicit b:RasterBuilder[T, T]):Raster[T] = {
    val array =
      (for(i <- 0 until raster.cols; j <- 0 until raster.rows; r1 = this(i, j); r2 = raster(i, j))
      yield if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)).toArray

    Raster(array, raster.cols, raster.rows)
  }
}
