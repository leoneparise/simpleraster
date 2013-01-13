package climadata.raster

import language.implicitConversions

trait IntImplicits {
  implicit def intRasterOps(lhs:Raster[Int]) = new {
    def +(rhs:Raster[Byte]):Raster[Int] = lhs + rhs.toInt
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs + rhs.toInt
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat + rhs
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble + rhs

    def +(rhs:Byte):Raster[Int] = lhs + IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Short):Raster[Int] = lhs + IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Int):Raster[Int] = lhs + IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Float):Raster[Float] = lhs.toFloat + FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def +(rhs:Double):Raster[Double] = lhs.toDouble + DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def -(rhs:Raster[Byte]):Raster[Int] = lhs - rhs.toInt
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs - rhs.toInt
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat - rhs
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble - rhs

    def -(rhs:Byte):Raster[Int] = lhs - IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Short):Raster[Int] = lhs - IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Int):Raster[Int] = lhs - IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Float):Raster[Float] = lhs.toFloat - FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def -(rhs:Double):Raster[Double] = lhs.toDouble - DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def *(rhs:Raster[Byte]):Raster[Int] = lhs * rhs.toInt
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs * rhs.toInt
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat * rhs
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble * rhs

    def *(rhs:Byte):Raster[Int] = lhs * IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Short):Raster[Int] = lhs * IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Int):Raster[Int] = lhs * IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Float):Raster[Float] = lhs.toFloat * FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def *(rhs:Double):Raster[Double] = lhs.toDouble * DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def /(rhs:Raster[Byte]):Raster[Int] = lhs / rhs.toInt
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs / rhs.toInt
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI):Raster[Float] = lhs.toFloat / rhs
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI):Raster[Double] = lhs.toDouble / rhs

    def /(rhs:Byte):Raster[Int] = lhs / IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Short):Raster[Int] = lhs / IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Int):Raster[Int] = lhs / IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Float):Raster[Float] = lhs.toFloat / FloatConstantRaster(rhs, lhs.rows, lhs.cols)
    def /(rhs:Double):Raster[Double] = lhs.toDouble / DoubleConstantRaster(rhs, lhs.rows, lhs.cols)

    def %(rhs:Raster[Byte]):Raster[Int] = lhs % rhs.toInt
    def %(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs % rhs.toInt

    def %(rhs:Byte):Raster[Int] = lhs % IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def %(rhs:Short):Raster[Int] = lhs % IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def %(rhs:Int):Raster[Int] = lhs % IntConstantRaster(rhs, lhs.rows, lhs.cols)

    def |(rhs:Raster[Byte]):Raster[Int] = lhs | rhs.toInt
    def |(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs | rhs.toInt

    def |(rhs:Byte):Raster[Int] = lhs | IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def |(rhs:Short):Raster[Int] = lhs | IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def |(rhs:Int):Raster[Int] = lhs | IntConstantRaster(rhs, lhs.rows, lhs.cols)

    def &(rhs:Raster[Byte]):Raster[Int] = lhs & rhs.toInt
    def &(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = lhs & rhs.toInt

    def &(rhs:Byte):Raster[Int] = lhs & IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def &(rhs:Short):Raster[Int] = lhs & IntConstantRaster(rhs, lhs.rows, lhs.cols)
    def &(rhs:Int):Raster[Int] = lhs & IntConstantRaster(rhs, lhs.rows, lhs.cols)
  }

  implicit def int2rasterOps(lhs:Int) = new {
    def +(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toInt
    def +(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) + rhs.toInt
    def +(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) + rhs
    def +(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) + rhs

    def -(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toInt
    def -(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) - rhs.toInt
    def -(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) - rhs
    def -(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) - rhs

    def *(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toInt
    def *(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) * rhs.toInt
    def *(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) * rhs
    def *(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) * rhs

    def /(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toInt
    def /(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) / rhs.toInt
    def /(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Float])(implicit i1:DI, i2:DI, i3:DI):Raster[Float] = new FloatConstantRaster(lhs.toFloat, rhs.rows, rhs.cols) / rhs
    def /(rhs:Raster[Double])(implicit i1:DI, i2:DI, i3:DI, i4:DI):Raster[Double] = new DoubleConstantRaster(lhs.toDouble, rhs.rows, rhs.cols) / rhs

    def %(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) % rhs.toInt
    def %(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) % rhs.toInt
    def %(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) % rhs

    def |(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) | rhs.toInt
    def |(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) | rhs.toInt
    def |(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) | rhs

    def &(rhs:Raster[Byte]):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) & rhs.toInt
    def &(rhs:Raster[Short])(implicit i1:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) & rhs.toInt
    def &(rhs:Raster[Int])(implicit i1:DI, i2:DI):Raster[Int] = new IntConstantRaster(lhs, rhs.rows, rhs.cols) & rhs
  }
}
