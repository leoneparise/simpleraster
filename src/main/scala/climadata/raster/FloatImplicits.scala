package climadata.raster

import language.implicitConversions

trait FloatImplicits {
  implicit def floatRasterOps(lhs:Raster[Float]) = new {
    def +(rhs:Raster[Byte]):Raster[Float] = lhs + rhs.toFloat
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = lhs + rhs.toFloat
    def +(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = lhs + rhs.toFloat
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble + rhs

    def +(rhs:Byte):Raster[Float] = lhs + FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def +(rhs:Short):Raster[Float] = lhs + FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def +(rhs:Int):Raster[Float] = lhs + FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def +(rhs:Float):Raster[Float] = lhs + FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Double):Raster[Double] = lhs.toDouble + DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def -(rhs:Raster[Byte]):Raster[Float] = lhs - rhs.toFloat
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = lhs - rhs.toFloat
    def -(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = lhs - rhs.toFloat
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble - rhs

    def -(rhs:Byte):Raster[Float] = lhs - FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def -(rhs:Short):Raster[Float] = lhs - FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def -(rhs:Int):Raster[Float] = lhs - FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def -(rhs:Float):Raster[Float] = lhs - FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Double):Raster[Double] = lhs.toDouble - DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def *(rhs:Raster[Byte]):Raster[Float] = lhs * rhs.toFloat
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = lhs * rhs.toFloat
    def *(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = lhs * rhs.toFloat
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble * rhs

    def *(rhs:Byte):Raster[Float] = lhs * FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def *(rhs:Short):Raster[Float] = lhs * FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def *(rhs:Int):Raster[Float] = lhs * FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def *(rhs:Float):Raster[Float] = lhs * FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Double):Raster[Double] = lhs.toDouble * DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def /(rhs:Raster[Byte]):Raster[Float] = lhs / rhs.toFloat
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = lhs / rhs.toFloat
    def /(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = lhs / rhs.toFloat
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble / rhs

    def /(rhs:Byte):Raster[Float] = lhs / FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def /(rhs:Short):Raster[Float] = lhs / FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def /(rhs:Int):Raster[Float] = lhs / FloatConstantRaster(rhs.toFloat, lhs.rows, lhs.cols)
    def /(rhs:Float):Raster[Float] = lhs / FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Double):Raster[Double] = lhs.toDouble / DoubleConstantRaster(rhs, lhs.rows, lhs.cols)
  }

  implicit def float2rasterOps(lhs:Float) = new {
    def +(rhs:Raster[Byte]):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toFloat
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toFloat
    def +(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toFloat
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) + rhs

    def -(rhs:Raster[Byte]):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toFloat
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toFloat
    def -(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toFloat
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) - rhs

    def *(rhs:Raster[Byte]):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toFloat
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toFloat
    def *(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toFloat
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) * rhs

    def /(rhs:Raster[Byte]):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toFloat
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toFloat
    def /(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toFloat
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) / rhs
  }
}
