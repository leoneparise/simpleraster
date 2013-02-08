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

  def combine(raster: Raster[Byte])(f: (Byte, Byte) => Byte)(implicit nd: NoData[Byte]): Raster[Byte] = {
    val array = Array.ofDim[Byte](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(nd.isNodata(r1) || nd.isNodata(r2)) nd.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Byte) => B)(implicit nd: NoData[Byte]):B =
    data.foldLeft(a0)((a, e) => if(nd.isNodata(e)) a else f(a, e))

  def unary_-(implicit b:RasterBuilder[Byte, Byte]) = map(e => (-e).toByte)

  def unary_~(implicit b:RasterBuilder[Byte, Double]) = map(e => 1.0/e)
}

class ShortConstantRaster(val const:Short) extends ConstantRaster[Short] {
  def data: Array[Short] = Array(const)

  def apply(row: Int, col: Int): Short = const

  def apply(i: Int):Short = const

  def combine(raster: Raster[Short])(f: (Short, Short) => Short)(implicit nd: NoData[Short]): Raster[Short] = {
    val array = Array.ofDim[Short](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(nd.isNodata(r1) || nd.isNodata(r2)) nd.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Short) => B)(implicit nd: NoData[Short]):B =
    data.foldLeft(a0)((a, e) => if(nd.isNodata(e)) a else f(a, e))

  def unary_-(implicit b:RasterBuilder[Short, Short]) = map(e => (-e).toShort)

  def unary_~(implicit b:RasterBuilder[Short, Double]) = map(e => 1.0/e)
}

class IntConstantRaster(val const:Int) extends ConstantRaster[Int] {
  def data: Array[Int] = Array(const)

  def apply(row: Int, col: Int): Int = const

  def apply(i: Int):Int = const

  def combine(raster: Raster[Int])(f: (Int, Int) => Int)(implicit nd: NoData[Int]): Raster[Int] = {
    val array = Array.ofDim[Int](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(nd.isNodata(r1) || nd.isNodata(r2)) nd.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Int) => B)(implicit nd: NoData[Int]):B =
    data.foldLeft(a0)((a, e) => if(nd.isNodata(e)) a else f(a, e))

  def unary_-(implicit b:RasterBuilder[Int, Int]) = map(e => -e)

  def unary_~(implicit b:RasterBuilder[Int, Double]) = map(e => 1.0/e)
}

class FloatConstantRaster(val const:Float) extends ConstantRaster[Float] {
  def data: Array[Float] = Array(const)

  def apply(row: Int, col: Int): Float = const

  def apply(i: Int):Float = const

  def combine(raster: Raster[Float])(f: (Float, Float) => Float)(implicit nd: NoData[Float]): Raster[Float] = {
    val array = Array.ofDim[Float](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(nd.isNodata(r1) || nd.isNodata(r2)) nd.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Float) => B)(implicit nd: NoData[Float]):B =
    data.foldLeft(a0)((a, e) => if(nd.isNodata(e)) a else f(a, e))

  def unary_-(implicit b:RasterBuilder[Float, Float]) = map(e => -e)

  def unary_~(implicit b:RasterBuilder[Float, Double]) = map(e => 1.0/e)
}

class DoubleConstantRaster(val const:Double) extends ConstantRaster[Double] {
  def data: Array[Double] = Array(const)

  def apply(row: Int, col: Int): Double = const

  def apply(i: Int):Double = const

  def combine(raster: Raster[Double])(f: (Double, Double) => Double)(implicit nd: NoData[Double]): Raster[Double] = {
    val array = Array.ofDim[Double](raster.size)
    var i = 0
    while(i < raster.size) {
      val r1 = this(i)
      val r2 = raster(i)
      array(i) = if(nd.isNodata(r1) || nd.isNodata(r2)) nd.nodata else f(r1, r2)
      i += 1
    }

    Raster(array, raster.rows, raster.cols)
  }

  def foldLeft[B](a0: B)(f: (B, Double) => B)(implicit nd: NoData[Double]):B =
    data.foldLeft(a0)((a, e) => if(nd.isNodata(e)) a else f(a, e))

  def unary_-(implicit b:RasterBuilder[Double, Double]) = map(e => -e)

  def unary_~(implicit b:RasterBuilder[Double, Double]) = map(e => 1.0/e)
}