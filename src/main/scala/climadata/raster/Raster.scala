package climadata.raster

import scala.language.implicitConversions

object Raster {
  def apply(data:Array[Byte], cols:Int, rows:Int):Raster[Byte] =
    new ByteRaster(data, cols, rows)

  def apply(data:Array[Short], cols:Int, rows:Int)(implicit i1:DI):Raster[Short] =
    new ShortRaster(data, cols, rows)

  def apply(data:Array[Int], cols:Int, rows:Int)(implicit i1:DI, i2:DI):Raster[Int] =
    new IntRaster(data, cols, rows)

  def apply(data:Array[Float], cols:Int, rows:Int)(implicit i1:DI, i2:DI, i3:DI):Raster[Float] =
    new FloatRaster(data, cols, rows)

  def apply(data:Array[Double], cols:Int, rows:Int)(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] =
    new DoubleRaster(data, cols, rows)

  def apply(const:Byte):Raster[Byte] = new ByteConstantRaster(const)

  def apply(const:Short):Raster[Short] = new ShortConstantRaster(const)

  def apply(const:Int):Raster[Int] = new IntConstantRaster(const)

  def apply(const:Float):Raster[Float] = new FloatConstantRaster(const)

  def apply(const:Double):Raster[Double] = new DoubleConstantRaster(const)
}

trait Raster[@specialized T] {
  def data:Array[T]

  def cols:Int

  def rows:Int

  def size = cols * rows

  if (data.length != size)
    throw new IllegalArgumentException("Data size and raster size must be equals")

  def apply(row:Int, col:Int):T

  def apply(i: Int):T

  def map[B](f:T => B)(implicit b:RasterBuilder[T, B]):Raster[B] = b.map(this, f)

  def combine(raster:Raster[T])(f: (T, T) => T)(implicit b:RasterBuilder[T, T]):Raster[T]

  def foldLeft[B](a0: B)(f: (B, T) => B)(implicit b:RasterBuilder[T, B]):B

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

class ByteRaster(val data:Array[Byte], val cols:Int, val rows:Int) extends Raster[Byte] {
  def apply(row: Int, col: Int): Byte = data(row * cols + col)

  def apply(i: Int): Byte = data(i)

  def combine(raster: Raster[Byte])(f: (Byte, Byte) => Byte)(implicit b: RasterBuilder[Byte, Byte]): Raster[Byte] = {
    val array = Array.ofDim[Byte](size)
    var i = 0
    while(i < size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, rows, cols)
  }

  def foldLeft[B](a0: B)(f: (B, Byte) => B)(implicit b:RasterBuilder[Byte, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class ShortRaster(val data:Array[Short], val cols:Int, val rows:Int) extends Raster[Short] {
  def apply(row: Int, col: Int): Short = data(row * cols + col)

  def apply(i: Int): Short = data(i)

  def combine(raster: Raster[Short])(f: (Short, Short) => Short)(implicit b: RasterBuilder[Short, Short]): Raster[Short] = {
    val array = Array.ofDim[Short](size)
    var i = 0
    while(i < size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, rows, cols)
  }

  def foldLeft[B](a0: B)(f: (B, Short) => B)(implicit b:RasterBuilder[Short, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class IntRaster(val data:Array[Int], val cols:Int, val rows:Int) extends Raster[Int] {
  def apply(row: Int, col: Int): Int = data(row * cols + col)

  def apply(i: Int): Int = data(i)

  def combine(raster: Raster[Int])(f: (Int, Int) => Int)(implicit b: RasterBuilder[Int, Int]): Raster[Int] = {
    val array = Array.ofDim[Int](size)
    var i = 0
    while(i < size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, rows, cols)
  }

  def foldLeft[B](a0: B)(f: (B, Int) => B)(implicit b:RasterBuilder[Int, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class FloatRaster(val data:Array[Float], val cols:Int, val rows:Int) extends Raster[Float] {
  def apply(row: Int, col: Int): Float = data(row * cols + col)

  def apply(i: Int): Float = data(i)

  def combine(raster: Raster[Float])(f: (Float, Float) => Float)(implicit b: RasterBuilder[Float, Float]): Raster[Float] = {
    val array = Array.ofDim[Float](size)
    var i = 0
    while(i < size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, rows, cols)
  }

  def foldLeft[B](a0: B)(f: (B, Float) => B)(implicit b:RasterBuilder[Float, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class DoubleRaster(val data:Array[Double], val cols:Int, val rows:Int) extends Raster[Double] {
  def apply(row: Int, col: Int): Double = data(row * cols + col)

  def apply(i: Int): Double = data(i)

  def combine(raster: Raster[Double])(f: (Double, Double) => Double)(implicit b: RasterBuilder[Double, Double]): Raster[Double] = {
    val array = Array.ofDim[Double](size)
    var i = 0
    while(i < size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, rows, cols)
  }

  def foldLeft[B](a0: B)(f: (B, Double) => B)(implicit b:RasterBuilder[Double, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}
