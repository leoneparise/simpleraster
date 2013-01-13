package climadata.raster

import language.implicitConversions

trait DoubleImplicits {
  implicit def doubleRasterOps(lhs:Raster[Double]) = new {
    def +(rhs:Raster[Byte]):Raster[Double] = lhs + rhs.toDouble
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = lhs + rhs.toDouble
    def +(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = lhs + rhs.toDouble
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs + rhs.toDouble

    def +(rhs:Byte):Raster[Double] = lhs + DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def +(rhs:Short):Raster[Double] = lhs + DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def +(rhs:Int):Raster[Double] = lhs + DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def +(rhs:Float):Raster[Double] = lhs + DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def +(rhs:Double):Raster[Double] = lhs + DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def -(rhs:Raster[Byte]):Raster[Double] = lhs - rhs.toDouble
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = lhs - rhs.toDouble
    def -(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = lhs - rhs.toDouble
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs - rhs.toDouble

    def -(rhs:Byte):Raster[Double] = lhs - DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def -(rhs:Short):Raster[Double] = lhs - DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def -(rhs:Int):Raster[Double] = lhs - DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def -(rhs:Float):Raster[Double] = lhs - DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def -(rhs:Double):Raster[Double] = lhs - DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def *(rhs:Raster[Byte]):Raster[Double] = lhs * rhs.toDouble
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = lhs * rhs.toDouble
    def *(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = lhs * rhs.toDouble
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs * rhs.toDouble

    def *(rhs:Byte):Raster[Double] = lhs * DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def *(rhs:Short):Raster[Double] = lhs * DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def *(rhs:Int):Raster[Double] = lhs * DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def *(rhs:Float):Raster[Double] = lhs * DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def *(rhs:Double):Raster[Double] = lhs * DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def /(rhs:Raster[Byte]):Raster[Double] = lhs / rhs.toDouble
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = lhs / rhs.toDouble
    def /(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = lhs / rhs.toDouble
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs / rhs.toDouble

    def /(rhs:Byte):Raster[Double] = lhs / DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def /(rhs:Short):Raster[Double] = lhs / DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def /(rhs:Int):Raster[Double] = lhs / DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def /(rhs:Float):Raster[Double] = lhs / DoubleConstantRaster(rhs.toDouble, lhs.rows, lhs.cols)
    def /(rhs:Double):Raster[Double] = lhs / DoubleConstantRaster(rhs, lhs.rows, lhs.cols)
  }

  implicit def double2rasterOps(lhs:Double) = new {
    def +(rhs:Raster[Byte]):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toDouble
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toDouble
    def +(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toDouble
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toDouble
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) + rhs

    def -(rhs:Raster[Byte]):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toDouble
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toDouble
    def -(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toDouble
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toDouble
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) - rhs

    def *(rhs:Raster[Byte]):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toDouble
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toDouble
    def *(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toDouble
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toDouble
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) * rhs

    def /(rhs:Raster[Byte]):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toDouble
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toDouble
    def /(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toDouble
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toDouble
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs, rhs.rows, rhs.cols) / rhs
  }
}
