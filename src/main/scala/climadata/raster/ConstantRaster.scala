package climadata.raster

trait ConstantRaster[@specialized T] extends Raster[T] {
  def const:T

  def cols:Int = 1
  def rows:Int = 1
}

class ByteConstantRaster(val const:Byte) extends ConstantRaster[Byte] {
  def data: Array[Byte] = Array(const)

  def apply(row: Int, col: Int): Byte = const

  def apply(i: Int):Byte = const

  def combine(raster: Raster[Byte])(f: (Byte, Byte) => Byte)(implicit b: RasterBuilder[Byte, Byte]): Raster[Byte] = {
    val array = Array.ofDim[Byte](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Byte) => B)(implicit b:RasterBuilder[Byte, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class ShortConstantRaster(val const:Short) extends ConstantRaster[Short] {
  def data: Array[Short] = Array(const)

  def apply(row: Int, col: Int): Short = const

  def apply(i: Int):Short = const

  def combine(raster: Raster[Short])(f: (Short, Short) => Short)(implicit b: RasterBuilder[Short, Short]): Raster[Short] = {
    val array = Array.ofDim[Short](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Short) => B)(implicit b:RasterBuilder[Short, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class IntConstantRaster(val const:Int) extends ConstantRaster[Int] {
  def data: Array[Int] = Array(const)

  def apply(row: Int, col: Int): Int = const

  def apply(i: Int):Int = const

  def combine(raster: Raster[Int])(f: (Int, Int) => Int)(implicit b: RasterBuilder[Int, Int]): Raster[Int] = {
    val array = Array.ofDim[Int](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Int) => B)(implicit b:RasterBuilder[Int, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class FloatConstantRaster(val const:Float) extends ConstantRaster[Float] {
  def data: Array[Float] = Array(const)

  def apply(row: Int, col: Int): Float = const

  def apply(i: Int):Float = const

  def combine(raster: Raster[Float])(f: (Float, Float) => Float)(implicit b: RasterBuilder[Float, Float]): Raster[Float] = {
    val array = Array.ofDim[Float](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Float) => B)(implicit b:RasterBuilder[Float, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}

class DoubleConstantRaster(val const:Double) extends ConstantRaster[Double] {
  def data: Array[Double] = Array(const)

  def apply(row: Int, col: Int): Double = const

  def apply(i: Int):Double = const

  def combine(raster: Raster[Double])(f: (Double, Double) => Double)(implicit b: RasterBuilder[Double, Double]): Raster[Double] = {
    val array = Array.ofDim[Double](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(b.isNodata(r1) || b.isNodata(r2)) b.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Double) => B)(implicit b:RasterBuilder[Double, B]):B =
    data.foldLeft(a0)((a, e) => if(b.isNodata(e)) a else f(a, e))
}